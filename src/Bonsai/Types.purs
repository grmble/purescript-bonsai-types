-- | This module defines the central Bonsai types
-- | that are used in the Core and VirtualDom modules.
module Bonsai.Types
  ( BONSAI
  , Cmd(..)
  , TaskContext
  , delayUntilRendered
  , emitMessage
  , emittingTask
  , simpleTask
  , unitTask
  , unsafeCoerceCmd
  )
where

import Prelude

import Bonsai.DOM (Document)
import Control.Alt (class Alt)
import Control.Monad.Aff (Aff, Fiber, forkAff, joinFiber, launchAff_)
import Control.Monad.Aff.AVar (AVar)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Class (liftEff)
import Control.Plus (class Plus)
import Data.Foldable (fold, for_)
import Data.Monoid (class Monoid)
import Unsafe.Coerce (unsafeCoerce)


-- | Effect for public types
foreign import data BONSAI :: Effect

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
-- |
-- | TODO: parallel apply
data Cmd eff msg
  = Cmd (Array msg)
  | TaskCmd (TaskContext eff msg -> Aff eff Unit)

-- Cmd is a functor so VNodes/Events can be mapped
instance cmdFunctor :: Functor (Cmd eff) where
  -- use liftA1 to test bind instance
  map = liftA1
  {--
  map f (Cmd ms) =
    Cmd $ map f ms
  map f (TaskCmd task) =
    TaskCmd $ mapTask f task
  --}

instance cmdApply :: Apply (Cmd eff) where
  apply = ap

instance cmdApplicative :: Applicative (Cmd eff) where
  pure x = Cmd [x]

instance cmdBind :: Bind (Cmd eff) where
  bind = bindCmd

instance cmdMonad :: Monad (Cmd eff)

mapTask
  :: forall eff a b
  .  (a -> b)
  -> (TaskContext eff a -> Aff eff Unit)
  -> (TaskContext eff b -> Aff eff Unit)
mapTask f ta contextB =
  ta (contextB { emitter = \a -> contextB.emitter $ f a })


bindCmd :: forall eff a b. Cmd eff a -> (a -> Cmd eff b) -> Cmd eff b
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
emitMessage :: forall aff msg. TaskContext aff msg -> msg -> Aff aff Unit
emitMessage ctx msg =
  unsafeCoerceAff $ liftEff $ ctx.emitter msg


-- | Delay the task until after the next render.
delayUntilRendered :: forall eff aff msg. TaskContext eff msg -> Aff aff Unit
delayUntilRendered ctx =
  unsafeCoerceAff $ ctx.delay


-- | Produces a simple task (not cancellable, ony emits the return values
simpleTask :: forall aff msg. Aff aff msg -> Cmd aff msg
simpleTask aff =
  TaskCmd $ \ctx ->
    aff >>= emitMessage ctx


-- | Procudes a task that can emit multiple times
emittingTask
  :: forall aff msg
  .  (TaskContext aff msg -> Aff aff Unit)
  -> Cmd aff msg
emittingTask = TaskCmd


-- | An effectful task without return value - e.g. write to storage, ...
unitTask :: forall aff msg. Aff aff Unit -> Cmd aff msg
unitTask aff =
  TaskCmd $ \_ -> aff



-- | Semigroup instance for Cmd
-- |
-- | aside from the obvious (combining commands with <>)
-- | this will also make (Tuple (Cmd eff) Model)
-- | an applicative functor (= the results of update functions)
instance semigroupCmd :: Semigroup (Cmd eff msg) where
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


instance monoidCmd :: Monoid (Cmd eff msg) where
  mempty = Cmd []


-- alt behaves as a sort of 'stream union', the commands
-- are executed in parallel, messages are emitted as they arrive
instance cmdAlt :: Alt (Cmd eff) where
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


instance cmdPlus :: Plus (Cmd eff) where
  empty = Cmd []


-- | Unsafe coerce the effects of a Cmd.
unsafeCoerceCmd :: forall eff1 eff2 msg. Cmd eff1 msg -> Cmd eff2 msg
unsafeCoerceCmd cmd = unsafeCoerce cmd



-- | The Task Context holds the emitter function for the task
-- |
-- | `emitter` is a function that will queue the message
-- | for processing.  `delay` is a function that will
-- | delay until the next render, if the model is currently dirty.
-- | `fiber` will be filled in with the aff fiber - it
-- | can be used to cancel the task later on.
type TaskContext eff msg =
  { emitter :: msg -> Eff eff Unit
  , delay :: Aff eff Unit
  , fiber :: AVar (Fiber eff Unit)
  , document :: Document
  }
