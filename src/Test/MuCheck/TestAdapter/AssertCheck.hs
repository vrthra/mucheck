-- | Module for using demonstration of using the TestAdapter
module Test.MuCheck.TestAdapter.AssertCheck where
import Control.Exception

data AssertStatus = AssertSuccess
                  | AssertFailure
  deriving (Eq, Show)

assertCheck :: Bool -> AssertStatus
assertCheck fn = case fn of
                  True -> AssertSuccess
                  False -> AssertFailure

assertCheckResult :: AssertStatus -> IO AssertStatus
assertCheckResult fn = withCheck $ case fn of
                                      AssertSuccess -> do  putStrLn "Success"
                                                           return AssertSuccess
                                      AssertFailure -> do  putStrLn "Failed"
                                                           return AssertFailure
  where withCheck f = catch f $ \e -> do   putStrLn $ show (e :: SomeException)
                                           return AssertFailure

