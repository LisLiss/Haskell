import Test11
import Test12
import Test13
import Test22
import Test32
import Test33
import Test34
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "All tests here"
      [ test11
      , test12
      , test13
      , test22
      , test32
      , test33
      , test34
      ]