-- | This module defines the central Bonsai types
-- | that are used in the Core and VirtualDom modules.
module Bonsai.Types
  ( Cmd(..)
  , TaskContext
  , delayUntilRendered
  , emitMessage
  , emittingTask
  , simpleTask
  , unitTask
  )
where

import Prelude

import Bonsai.DOM (Document)
import Control.Alt (class Alt)
import Control.Plus (class Plus)
import Data.Foldable (fold, for_)
import Effect (Effect)
import Effect.Aff (Aff, Fiber, forkAff, joinFiber, launchAff_)
import Effect.Aff.AVar (AVar)
import Effect.Class (liftEffect)


-- | A Command represents messages that should be applied to the Bonsai model
-- |
-- | A command is either Pure (the constructor Cmd)
-- | or an asynchronous Task.  The pure command simply contains
-- | the messages that will be emitted.
-- | The asynchronous Task gets an emitter function
-- | that it can use to emit messages at will.
-- |
-- | Cmd MUST be a Functor, so the type can be mapped
-- | by the virtual dom.
-- |
-- | It is also a Monoid, this means commands can be combined by `<>`.
-- | The combined commands are issued in sequence.
-- |
-- | It is also a Monad and Plus, this gives all the monad goodies plus
-- | pure and empty.  `alt` executes it's commands in parallel,
-- | messages are emitted as they arrive.
data Cmd msg
  = Cmd (Array msg)
  | TaskCmd (TaskContext msg -> Aff Unit)

-- Cmd is a functor so VNodes/Events can be mapped
instance cmdFunctor :: Functor Cmd  where
  map = liftA1

instance cmdApply :: Apply Cmd where
  apply = ap

instance cmdApplicative :: Applicative Cmd where
  pure x = Cmd [x]

instance cmdBind :: Bind Cmd where
  bind = bindCmd

instance cmdMonad :: Monad Cmd


bindCmd :: forall a b. Cmd a -> (a -> Cmd b) -> Cmd b
bindCmd (Cmd []) _ = Cmd []
bindCmd (Cmd as) f =
  fold $ map f as
bindCmd (TaskCmd ta) faCb =
  TaskCmd \contextB ->
    let emitterA a =
          case faCb a of
            Cmd bs -> do
              for_ bs contextB.emitter
            TaskCmd tb ->
              launchAff_ $ tb contextB
    in ta (contextB { emitter = emitterA })


-- | Emit helper for Tasks.
-- |
-- | In an emitting task, use this function to emit messages.
emitMessage :: forall msg. TaskContext msg -> msg -> Aff Unit
emitMessage ctx msg =
  liftEffect $ ctx.emitter msg


-- | Delay the task until after the next render.
delayUntilRendered :: forall msg. TaskContext msg -> Aff Unit
delayUntilRendered ctx =
  ctx.delay


-- | Produces a simple task
-- |
-- | A simple task is not cancelable and it can only emit one
-- | single message - the return value returned by the Aff.
-- |
-- | It does get the Document, so it can do DOM manipulations
-- | and perform aribitrary aff operations, e.g. AJAX calls to
-- | a server.
simpleTask :: forall msg. (Document -> Aff msg) -> Cmd msg
simpleTask aff =
  TaskCmd $ \ctx ->
    aff ctx.document >>= emitMessage ctx


-- | Procudes a task that can emit multiple times
-- |
-- | This type of task has access to the full task context,
-- | so it can emit as many of the given type as it wants to.
-- | (Don't `unsafeCoerce` the type - that will just break all the
-- | outer mapping functions).
-- |
-- | It can also access it's `Fiber`, so it can arrange for its own
-- | cancellation.
emittingTask
  :: forall msg
  .  (TaskContext msg -> Aff Unit)
  -> Cmd msg
emittingTask = TaskCmd


-- | An effectful task that does not emit a message.
-- |
-- | Even simpler than  the simple task, this one can
-- | not change the model in any way, it can only
-- | produce side effects.
-- |
-- | It does get the `Document`, so it can manipulate the DOM.
-- | It could also store the model in local storage.
unitTask :: forall msg. (Document -> Aff Unit) -> Cmd msg
unitTask aff =
  TaskCmd $ \ctx -> aff ctx.document



-- | Semigroup instance for Cmd
-- |
-- | aside from the obvious (combining commands with <>)
-- | this will also make (Tuple (Cmd eff) Model)
-- | an applicative functor (= the results of update functions)
instance semigroupCmd :: Semigroup (Cmd msg) where
  append (Cmd []) x =
    x
  append x (Cmd []) =
    x
  append (Cmd m1) (Cmd m2) =
    Cmd (m1 <> m2)
  append (Cmd m) (TaskCmd task) =
    TaskCmd \ctx -> do
      for_ m (emitMessage ctx)
      task ctx
  append (TaskCmd task) (Cmd m) =
    TaskCmd \ctx -> do
      task ctx
      for_ m (emitMessage ctx)
  append (TaskCmd t1) (TaskCmd t2) =
    TaskCmd \ctx -> do
      t1 ctx
      t2 ctx


instance monoidCmd :: Monoid (Cmd msg) where
  mempty = Cmd []


-- alt behaves as a sort of 'stream union', the commands
-- are executed in parallel, messages are emitted as they arrive
instance cmdAlt :: Alt Cmd where
  alt (TaskCmd task) (Cmd m) =
    TaskCmd \ctx -> do
      for_ m (emitMessage ctx)
      task ctx
  alt (TaskCmd t1) (TaskCmd t2) =
    TaskCmd \ctx -> do
      -- 'bhead is not a function'
      -- when trying to use bracket
      -- XXX fix it
      fib1 <- forkAff $ t1 ctx
      t2 ctx
      joinFiber fib1

  -- everything else is the same as append
  alt x y = append x y


instance cmdPlus :: Plus Cmd where
  empty = Cmd []



-- | The Task Context holds the emitter function for the task
-- |
-- | `emitter` is a function that will queue the message
-- | for processing.  `delay` is a function that will
-- | delay until the next render, if the model is currently dirty.
-- | `fiber` will be filled in with the aff fiber - it
-- | can be used to cancel the task later on.
type TaskContext msg =
  { emitter :: msg -> Effect Unit
  , delay :: Aff Unit
  , fiber :: AVar (Fiber Unit)
  , document :: Document
  }
