import Test_Task1
import Test_Task2
import Test_Task3
import Test_Task21
import Test_Task22
import Test_Task31
import Test_Task32
import Test.Tasty

main :: IO ()
main = 
    defaultMain $
     testGroup
        "Starting"
        [testTask1
        , testTask2
        , testTask3
        , testTask21
        , testTask22
        , testTask31
        , testTask32
        ]
