{-# OPTIONS_GHC -F -pgmF htfpp #-}
module NineNineProblemsTest where
import NineNineProblems

import Test.HUnit.Lang
import Test.Framework
import Data.List



test_myLast :: Assertion
test_myLast = do         
        let list = [1, 2, 3, 4, 5]
        assertEqual (myLast list) 5