module Block3.Task34 where


import Block3.Task31
import Block3.Task32 (satisfy, element, eof, ok, stream)
import Block3.Task33
import Control.Monad
import Control.Applicative

import Data.Char (isDigit, isSpace)

-- | Parser which skip all whitespaces
whitespacesParser :: Parser Char String
whitespacesParser = many (satisfy isSpace)

-- | Parser of array with integer numbers
listParser :: Parser Char [Int]
listParser = whitespacesParser *> (intParser >>= recFromSizeListParser) <* whitespacesParser
  where
    recFromSizeListParser :: Int -> Parser Char [Int]
    recFromSizeListParser n
      | n < 0 = empty
      | n == 0 = return []
      | otherwise = fmap (:) parseOneInt <*> recFromSizeListParser (n - 1)
    
    parseOneInt = whitespacesParser *> element ',' *> whitespacesParser *> intParser

-- | Parser of array with arrays of integer numbers
listlistParser :: Parser Char [[Int]]
listlistParser = many (readNext <|> readLast) <* eof
  where
    readNext = listParser <* element ','
    readLast = listParser <* eof
                                  

    