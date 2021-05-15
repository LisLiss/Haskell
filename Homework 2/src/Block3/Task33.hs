module Block3.Task33 where

import Block3.Task31
import Block3.Task32
import Control.Monad
import Control.Applicative
import Data.Char (isDigit, isSpace)

data BalancedSequences = 
  Empty
  | InBrackets BalancedSequences
  | Concat BalancedSequences BalancedSequences
  
instance Show BalancedSequences where
  show Empty = ""
  show (InBrackets x) = "(" ++ (show x) ++ ")"
  show (Concat x y) = (show x) ++ (show y)

-- | Consume only balanced bracket sequences (or fail otherwise)
balancedSequencesParser :: Parser Char BalancedSequences
balancedSequencesParser = checkParser <* eof
  where
    checkParser = checkBracket <|> (Empty <$ ok)
    checkBracket = do 
      let firstBracket = element '('
      let lastBracket = element ')'
      let isInBrackets = firstBracket *> checkParser <* lastBracket
      (fmap Concat (fmap InBrackets isInBrackets)) <*> checkParser 
      
-- | Make number out of character              
fromCharToNum :: Num t => Char -> t
fromCharToNum '1' = 1
fromCharToNum '2' = 2
fromCharToNum '3' = 3
fromCharToNum '4' = 4
fromCharToNum '5' = 5
fromCharToNum '6' = 6
fromCharToNum '7' = 7
fromCharToNum '8' = 8
fromCharToNum '9' = 9
fromCharToNum '0' = 0 
          
          
-- | Parser of integer numbers
intParser :: Num t => Parser Char t
intParser = withoutSignInt <|> withSignInt
  where
    withoutSign = do
      symbol <- satisfy isDigit
      (plusAns, powAns) <- withoutSign <|> ((0, 0) <$ ok)
      return ( (fromCharToNum symbol * (10 ^ powAns) + plusAns), (powAns + 1) )
    
    withoutSignInt = fst <$> withoutSign

    withSignInt = do
      sign <- element '+' <|> element '-'
      num <- fst <$> withoutSign
      if sign == '+' then return num else return (num * (-1))