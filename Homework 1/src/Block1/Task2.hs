{-# LANGUAGE InstanceSigs #-}

module Block1.Task2 where

data Nat = Z
          | S Nat
          deriving (Show)


instance Num Nat where
    -- |Summa operation for natural numbers
    (+) :: Nat -> Nat -> Nat
    x + Z = x
    Z + x = x
    S x + y = S (x + y)

    -- |Subtraction operation for natural numbers
    (-) :: Nat -> Nat -> Nat
    x - Z = x
    Z - x = Z
    (S x) - (S y) = x - y

    -- |Multiply operation for natural numbers
    (*) :: Nat -> Nat -> Nat
    x * Z = Z
    Z * x = Z
    S x * y = (x * y) + y
    
    -- |fromInteger operation for natural numbers
    fromInteger :: Integer -> Nat
    fromInteger x
      | x == 0    = Z
      | x < 0     = error "Can't be < 0"
      | otherwise = S (fromInteger (x - 1))
      
    abs :: Nat -> Nat
    abs x = x
    
    signum :: Nat -> Nat
    signum Z = Z
    signum x = S Z
    
-- |toInteger operation for natural numbers
fromNat :: Nat -> Integer
fromNat Z = 0
fromNat (S x) = fromNat x + 1

instance Eq Nat where
    -- |Equal checker for natural numbers
    (==):: Nat -> Nat -> Bool
    Z == Z = True
    (S x) == (S y) = x == y
    x == y = False

instance Ord Nat where
    -- |Ordering natural numbers
    compare :: Nat -> Nat -> Ordering
    compare (S x) (S y) = compare x y
    compare Z Z         = EQ
    compare Z x         = LT
    compare x Z         = GT
    
-- |Calculate whether natural number is even or odd
isEven :: Nat -> Bool
isEven Z = True
isEven (S x) = not (isEven x)

-- |Div operation for natural numbers
divNat :: Nat -> Nat -> Nat
divNat x y 
    | y == Z    = error "Divider is zero"
    | x >= y    = S (divNat (x - y) y) 
    | otherwise = Z  
    
-- |Mod operation for natural numbers
modNat :: Nat -> Nat -> Nat
modNat x y 
    | y == Z    = error "Divider is zero"
    | x >= y    = modNat (x - y) y 
    | otherwise = x   