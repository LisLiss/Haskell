module Block2.Task21 where

import Control.Monad

data ArithmeticError
  = DivisionByZero
  | NegativePower
  deriving (Show, Eq)

-- | Type of error, when the expression contain
-- Division by zero
divZero :: Int -> Either ArithmeticError Expr
divZero x
  | x == 0 = Left DivisionByZero
  | otherwise = Right (Const x)

-- | Type of error, when the expression contain
-- negative pow operation
powMinus :: Int -> Either ArithmeticError Expr
powMinus x
  | x < 0 = Left NegativePower
  | otherwise = Right (Const x)
  
  
-- | Every expression we can express with this type
-- | Some operations can throw errors, so we handle them
data Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Show, Eq)

-- | Implementation of liftM2 but with evaluating arguments of binary function
liftForEval :: (Int -> Int -> Int) -> Expr -> Expr -> Either ArithmeticError Int
liftForEval f x y = liftM2 f (eval x) (eval y)
 
-- | Evaluate recursively every argument of expression
-- | Down to its core
eval :: Expr -> Either ArithmeticError Int
eval (Const x) = Right x
eval (Add x y) = liftForEval (+) x y
eval (Sub x y) = liftForEval (-) x y
eval (Mul x y) = liftForEval (*) x y
eval (Div x y) = eval y >>= divZero >>= liftForEval div x
eval (Pow x y) = eval y >>= powMinus >>= liftForEval (^) x


