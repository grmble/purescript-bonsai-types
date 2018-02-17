module Test.Cmd where

import Prelude

import Bonsai.DOM (DOM, Document, affF)
import Bonsai.JSDOM (jsdomDocument)
import Bonsai.Types (Cmd(..), emitMessage, emittingTask, simpleTask, unitTask)
import Control.Monad.Aff (Aff, Milliseconds(..), delay, liftEff')
import Control.Monad.Aff.AVar (makeEmptyVar)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Free (Free)
import Control.Plus (empty, (<|>))
import Data.Array as Array
import Data.Foldable (for_)
import Data.Int (toNumber)
import Data.Monoid (mempty)
import Data.Tuple (Tuple(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

tests :: forall eff. Free (TestF (console::CONSOLE,dom::DOM,ref::REF|eff)) Unit
tests = do
  simpleTests
  monoidTests
  altTests
  monadTests


--
--
-- commands can emit multiple messages
-- they can also have arbitrary side effects
-- in our tests, the side effects will be
-- newtyped int that are appended to an
-- array.  they also delay for the same
-- number of milliseconds, to simulate
-- network delays or animations or ...
--
-- the message type is string, emitted messages
-- are collected as well
--
-- we fake the bonsai message processing code
-- here for a simplified test env
--

newtype SideEffect = SideEffect Int

derive newtype instance eqSideEffect :: Eq SideEffect
derive newtype instance showSideEffect :: Show SideEffect

runCmd
  :: forall eff
  .  Document
  -> Ref (Array String)
  -> Cmd eff String
  -> Aff eff Unit
runCmd _ msgs (Cmd ms) = do
  unsafeCoerceAff $ liftEff' $ modifyRef msgs (\xs -> xs <> ms)

runCmd doc msgs (TaskCmd task) = do
  fib <- unsafeCoerceAff $ makeEmptyVar
  let tc = { emitter : emitter
           , delay: pure unit
           , fiber: fib
           , document: doc
           }
  task tc

  where
    emitter msg =
      unsafeCoerceEff $ do
        -- log ("runCmd emitter: " <> msg)
        modifyRef msgs (\xs -> Array.snoc xs msg)
        -- weHave <- readRef msgs
        -- log ("we have: " <> show weHave)


-- | Run a Cmd and collect side effects and results
testCmd
  :: forall eff
  .  (Ref (Array SideEffect) -> Cmd eff String)
  -> Aff eff (Tuple (Array SideEffect) (Array String))
testCmd fn =
  testCmdDelay (Milliseconds 0.0) fn

-- | Run a Cmd and collect side effects and results
-- |
-- | Will delay before returning results ...
testCmdDelay
  :: forall eff
  .  Milliseconds
  -> (Ref (Array SideEffect) -> Cmd eff String)
  -> Aff eff (Tuple (Array SideEffect) (Array String))
testCmdDelay millis fn = do
  doc <- unsafeCoerceAff $ testDocument
  mref <- unsafeCoerceAff $ liftEff' $ newRef []
  sref <- unsafeCoerceAff $ liftEff' $ newRef []
  runCmd doc mref (fn sref)
  delay millis
  sides <- unsafeCoerceAff $ liftEff' $ readRef sref
  msgs <- unsafeCoerceAff $ liftEff' $ readRef mref
  pure $ Tuple sides msgs



sideEffect :: forall eff. Ref (Array SideEffect) -> SideEffect -> Aff (ref::REF|eff) Unit
sideEffect sref side@(SideEffect sideI) = do
  delay (Milliseconds $ toNumber sideI)
  liftEff' $ modifyRef sref (\xs -> Array.snoc xs side)

testDocument :: forall eff. Aff (dom::DOM|eff) Document
testDocument =
  affF $ jsdomDocument """<div id="main">Loading ...</div>"""

--
-- tests emitted messages/side effects for the Cmd primitives
--
simpleTests :: forall eff. Free (TestF (console::CONSOLE,dom::DOM,ref::REF|eff)) Unit
simpleTests =
  suite "simple" do
    test "empty" $ do
      Tuple sides msgs <- testCmd (const $ empty)
      Assert.equal [] sides
      Assert.equal [] msgs

    test "pure" $ do
      Tuple sides msgs <- testCmd (const $ pure "A")
      Assert.equal [] sides
      Assert.equal ["A"] msgs

    test "Cmd" $ do
      Tuple sides msgs <- testCmd (const $ Cmd ["A", "B"])
      Assert.equal [] sides
      Assert.equal ["A", "B"] msgs

    test "unitTask" $ do
      Tuple sides msgs <- testCmd \sref ->
        unitTask $ const $ sideEffect sref (SideEffect 1)
      Assert.equal [] msgs
      Assert.equal [ SideEffect 1 ] sides

    test "simpleTask" $ do
      Tuple sides msgs <- testCmd \sref ->
        simpleTask $ const do
          sideEffect sref (SideEffect 1)
          pure "A"
      Assert.equal [ "A" ] msgs
      Assert.equal [ SideEffect 1 ] sides

    test "emittingTask" $ do
      Tuple sides msgs <- testCmd \sref ->
        emittingTask \ctx -> do
          sideEffect sref (SideEffect 1)
          emitMessage ctx "A"
          sideEffect sref (SideEffect 2)
          emitMessage ctx "B"
      Assert.equal [ "A", "B" ] msgs
      Assert.equal [ SideEffect 1, SideEffect 2 ] sides


delayedCmd :: forall eff. Ref (Array SideEffect) -> Array Int -> Cmd eff String
delayedCmd sref delays =
  emittingTask \ctx -> unsafeCoerceAff $
    for_ delays \i -> do
      sideEffect sref (SideEffect i)
      unsafeCoerceAff $ emitMessage ctx (show i)

--
-- the monoid instance has string andThen behaviour
-- the commands are executed in sequence, the second
-- one is only started after the first one ends.
monoidTests :: forall eff. Free (TestF (console::CONSOLE,dom::DOM,ref::REF|eff)) Unit
monoidTests =
  suite "Monoid Cmd" do
    test "mempty<>pure" do
      Tuple sides msgs <- testCmd (const $ mempty <> pure "A")
      Assert.equal [ ] sides
      Assert.equal [ "A" ] msgs
    test "pure<>mempty" do
      Tuple sides msgs <- testCmd (const $ pure "A" <> mempty)
      Assert.equal [ ] sides
      Assert.equal [ "A" ] msgs
    test "pure<>pure" do
      Tuple sides msgs <- testCmd (const $ pure "A" <> pure "B")
      Assert.equal [ ] sides
      Assert.equal [ "A", "B" ] msgs
    test "Cmd<>Cmd" do
      Tuple sides msgs <- testCmd (const $ Cmd ["A", "B"] <> Cmd ["C", "D"])
      Assert.equal [ ] sides
      Assert.equal [ "A", "B", "C", "D" ] msgs
    test "mempty<>task" do
      Tuple sides msgs <- testCmd \sref ->
        mempty <> delayedCmd sref [1, 2]
      Assert.equal [ SideEffect 1, SideEffect 2] sides
      Assert.equal [ "1", "2"] msgs
    test "task<>mempty" do
      Tuple sides msgs <- testCmd \sref ->
        mempty <> delayedCmd sref [1, 2]
      Assert.equal [ SideEffect 1, SideEffect 2] sides
      Assert.equal [ "1", "2"] msgs
    test "cmd<>task" do
      Tuple sides msgs <- testCmd \sref ->
        Cmd ["A", "B"] <> delayedCmd sref [10, 20]
      Assert.equal [ SideEffect 10, SideEffect 20] sides
      Assert.equal [ "A", "B", "10", "20"] msgs
    test "task<>cmd" do
      Tuple sides msgs <- testCmd \sref ->
        delayedCmd sref [10, 20] <> Cmd ["A", "B"]
      Assert.equal [ SideEffect 10, SideEffect 20] sides
      Assert.equal [ "10", "20", "A", "B" ] msgs
    test "task<>task" do
      Tuple sides msgs <- testCmd \sref ->
        delayedCmd sref [10, 30] <> delayedCmd sref [20, 40]
      Assert.equal [ SideEffect 10, SideEffect 30, SideEffect 20, SideEffect 40] sides
      Assert.equal [ "10", "30", "20", "40"] msgs


-- <|> behaves as a stream union:  the commands are executed in
-- parallel, messages are emitted as they come in
altTests :: forall eff. Free (TestF (console::CONSOLE,dom::DOM,ref::REF|eff)) Unit
altTests =
  suite "Alt Cmd" do
    test "empty<|>pure" do
      Tuple sides msgs <- testCmd (const $ empty <|> pure "A")
      Assert.equal [ ] sides
      Assert.equal [ "A" ] msgs
    test "pure<|>empty" do
      Tuple sides msgs <- testCmd (const $ pure "A" <|> empty)
      Assert.equal [ ] sides
      Assert.equal [ "A" ] msgs
    test "pure<|>pure" do
      Tuple sides msgs <- testCmd (const $ pure "A" <|> pure "B")
      Assert.equal [ ] sides
      Assert.equal [ "A", "B" ] msgs
    test "Cmd<|>Cmd" do
      Tuple sides msgs <- testCmd (const $ Cmd ["A", "B"] <|> Cmd ["C", "D"])
      Assert.equal [ ] sides
      Assert.equal [ "A", "B", "C", "D" ] msgs
    test "empty<>task" do
      Tuple sides msgs <- testCmd \sref ->
        empty <|> delayedCmd sref [1, 2]
      Assert.equal [ SideEffect 1, SideEffect 2] sides
      Assert.equal [ "1", "2"] msgs
    test "task<>empty" do
      Tuple sides msgs <- testCmd \sref ->
        empty <|> delayedCmd sref [1, 2]
      Assert.equal [ SideEffect 1, SideEffect 2] sides
      Assert.equal [ "1", "2"] msgs
    test "cmd<|>task" do
      Tuple sides msgs <- testCmd \sref ->
        Cmd ["A", "B"] <|> delayedCmd sref [10, 20]
      Assert.equal [ SideEffect 10, SideEffect 20] sides
      Assert.equal [ "A", "B", "10", "20"] msgs
    test "task<|>cmd" do
      Tuple sides msgs <- testCmd \sref ->
        delayedCmd sref [10, 20] <|> Cmd ["A", "B"]
      Assert.equal [ SideEffect 10, SideEffect 20] sides
      Assert.equal [ "A", "B", "10", "20" ] msgs
    test "task<|>task" do
      Tuple sides msgs <- testCmd \sref ->
        delayedCmd sref [10, 30] <|> delayedCmd sref [20, 40]
      Assert.equal [ SideEffect 10, SideEffect 20, SideEffect 30, SideEffect 40] sides
      Assert.equal [ "10", "20", "30", "40"] msgs


monadTests :: forall eff. Free (TestF (console::CONSOLE,dom::DOM,ref::REF|eff)) Unit
monadTests =
  suite "Monad Cmd" do
    test "empty>>=" do
      Tuple sides msgs <- testCmd \sref -> do
        msg <- empty
        pure "X"
      Assert.equal [ ] sides
      Assert.equal [ ] msgs
    test "unitTask>>=" do
      -- sideEffects are executed until first empty
      Tuple sides msgs <- testCmd \sref -> do
        msg <- unitTask $ const (sideEffect sref (SideEffect 1))
        msg2 <- unitTask $ const (sideEffect sref (SideEffect 2))
        pure "X"
      Assert.equal [ SideEffect 1 ] sides
      Assert.equal [ ] msgs
    test "cmdDoCmd" do
      Tuple sides msgs <- testCmd \sref -> do
        msg1 <- Cmd ["a", "b"]
        msg2 <- Cmd ["A", "B"]
        pure (msg1 <> ":" <> msg2)
      Assert.equal [ ] sides
      Assert.equal [ "a:A", "a:B", "b:A", "b:B" ] msgs
    test "cmdDoTask" do
      Tuple sides msgs <- testCmd \sref -> do
        msg1 <- Cmd ["a", "b"]
        msg2 <- delayedCmd sref [10, 30]
        pure (msg1 <> ":" <> msg2)
      Assert.equal [ SideEffect 10, SideEffect 30,  SideEffect 10, SideEffect 30 ] sides
      Assert.equal [ "a:10", "a:30", "b:10", "b:30" ] msgs
    test "taskDoCmd" do
      Tuple sides msgs <- testCmd \sref -> do
        msg1 <- delayedCmd sref [10, 30]
        msg2 <- Cmd ["A", "B"]
        pure (msg1 <> ":" <> msg2)
      -- outer effect gets evaluated only once
      Assert.equal [ SideEffect 10, SideEffect 30 ] sides
      Assert.equal [ "10:A", "10:B", "30:A", "30:B" ] msgs
    test "taskDoTask" do
      Tuple sides msgs <- testCmdDelay (Milliseconds 200.0) \sref -> do
        msg1 <- delayedCmd sref [10, 30]
        msg2 <- delayedCmd sref [20, 40]
        pure (msg1 <> ":" <> msg2)
      -- the order here is determined by the delays, and 10/30 are in the outer loop
      Assert.equal (map SideEffect [ 10, 20, 30, 20, 40, 40 ]) sides
      -- the delays from the line above drive the order the combinations arrive in
      Assert.equal [ "10:20", "30:20", "10:40", "30:40" ] msgs
