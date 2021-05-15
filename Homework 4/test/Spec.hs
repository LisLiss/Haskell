import Task1Tests
import Task2Tests
import Task3Tests
import Task6Tests
import Task7Tests

import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "Test block" [tests1
                                           , tests2
                                           , tests3
                                           , tests6
                                           , tests7]
