module Test.Main where

import Prelude

import Bonsai.DOM (DOM, Document, affF)
import Bonsai.JSDOM (jsdomDocument)
import Bonsai.Types (Cmd(..), emitMessage, emittingTask, simpleTask, unitTask)
import Control.Monad.Aff (Aff, liftEff')
import Control.Monad.Aff.AVar (makeEmptyVar)
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, newRef, readRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Free (Free)
import Control.Plus (empty)
import Data.Array as Array
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall t1.
  Eff
    ( avar :: AVAR
    , console :: CONSOLE
    , dom :: DOM
    , exception :: EXCEPTION
    , ref :: REF
    , testOutput :: TESTOUTPUT
    | t1
    )
    Unit
main = runTest tests


--
--
-- commands can emit multiple messages
-- they can also have arbitrary side effects
-- in our tests, the side effects will be
-- newtyped strings that are appended to an
-- array
--
-- the message type is string, emitted messages
-- are collected as well
--
-- we fake the bonsai message processing code
-- here for a simplified test env
--

newtype SideEffect = SideEffect String

derive newtype instance eqSideEffect :: Eq SideEffect
derive newtype instance showSideEffect :: Show SideEffect

type Msg = String

runCmd
  :: forall eff
  .  Document
  -> Ref (Array Msg)
  -> Cmd eff Msg
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
      unsafeCoerceEff $ modifyRef msgs (\xs -> Array.snoc xs msg)


sideEffect :: forall eff. Ref (Array SideEffect) -> SideEffect -> Aff (ref::REF|eff) Unit
sideEffect sref side =
  liftEff' $ modifyRef sref (\xs -> Array.snoc xs side)

testDocument :: forall eff. Aff (dom::DOM|eff) Document
testDocument =
  affF $ jsdomDocument """<div id="main">Loading ...</div>"""

tests :: forall eff. Free (TestF (console::CONSOLE,dom::DOM,ref::REF|eff)) Unit
tests =
  suite "simple" do
    test "empty" $ do
      doc <- testDocument
      mref <- liftEff' $ newRef []
      runCmd doc mref empty
      msgs <- liftEff' $ readRef mref
      Assert.equal [] msgs

    test "pure" $ do
      doc <- testDocument
      mref <- liftEff' $ newRef []
      runCmd doc mref (pure "A")
      msgs <- liftEff' $ readRef mref
      Assert.equal ["A"] msgs

    test "Cmd" $ do
      doc <- testDocument
      mref <- liftEff' $ newRef []
      runCmd doc mref (Cmd ["A", "B"])
      msgs <- liftEff' $ readRef mref
      Assert.equal ["A", "B"] msgs

    test "unitTask" $ do
      doc <- testDocument
      mref <- liftEff' $ newRef []
      sref <- liftEff' $ newRef []
      runCmd doc mref $
        unitTask do
          sideEffect sref (SideEffect "#1")
      msgs <- liftEff' $ readRef mref
      sides <- liftEff' $ readRef sref
      Assert.equal [] msgs
      Assert.equal [ SideEffect "#1" ] sides

    test "simpleTask" $ do
      doc <- testDocument
      mref <- liftEff' $ newRef []
      sref <- liftEff' $ newRef []
      runCmd doc mref $
        simpleTask do
          sideEffect sref (SideEffect "#1")
          pure "A"
      msgs <- liftEff' $ readRef mref
      sides <- liftEff' $ readRef sref
      Assert.equal [ "A" ] msgs
      Assert.equal [ SideEffect "#1" ] sides

    test "emittingTask" $ do
      doc <- testDocument
      mref <- liftEff' $ newRef []
      sref <- liftEff' $ newRef []
      runCmd doc mref $
        emittingTask \ctx -> do
          sideEffect sref (SideEffect "#1")
          emitMessage ctx "A"
          sideEffect sref (SideEffect "#2")
          emitMessage ctx "B"
      msgs <- liftEff' $ readRef mref
      sides <- liftEff' $ readRef sref
      Assert.equal [ "A", "B" ] msgs
      Assert.equal [ SideEffect "#1", SideEffect "#2" ] sides
