module Test.Main where

import Prelude

import Effect (Effect)
import Control.Monad.Free (Free)
import Test.Cmd as TCmd
import Test.Unit (TestF)
import Test.Unit.Main (runTest)


main :: Effect Unit
main = runTest tests



tests :: Free TestF Unit
tests = do
  TCmd.tests
