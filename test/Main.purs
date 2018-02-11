module Test.Main where

import Prelude

import Bonsai.DOM (DOM)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Free (Free)
import Test.Cmd as TCmd
import Test.Unit (TestF)
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



tests :: forall eff. Free (TestF (console::CONSOLE,dom::DOM,ref::REF|eff)) Unit
tests = do
  TCmd.tests
