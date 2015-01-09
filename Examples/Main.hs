module Main where
import Test.MuCheck.TestAdapter.AssertCheck
import Examples.AssertCheckTest

main = do
  assertCheckResult sortEmpty
  assertCheckResult sortSorted
  assertCheckResult sortRev
  assertCheckResult sortSame
  assertCheckResult sortNeg
