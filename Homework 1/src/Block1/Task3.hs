{-# LANGUAGE InstanceSigs #-}

module Block1.Task3 where

import Data.List.NonEmpty as DLNonEmpty (NonEmpty (..), length, head, nonEmpty, cons, tail)
import Data.List as DList (group, length, sort, init, last)
import Data.Maybe (fromJust)

data Tree x = List 
            | Node (NonEmpty x) (Tree x) (Tree x)
            deriving (Show)

-- |isEmpty function for Tree
isEmptyTree :: Tree x -> Bool 
isEmptyTree List = True
isEmptyTree a    = False

-- |Calculate size of tree
sizeTree :: Tree x -> Int
sizeTree List = 0
sizeTree (Node node left right) = (DLNonEmpty.length node) + (sizeTree left) + (sizeTree right)

-- |Searching function in BST
findValue :: Ord x =>  Tree x -> x -> Bool
findValue List a = False
findValue (Node node left right) a 
    | f == a  = True
    | f < a   = findValue right a
    | f > a   = findValue left a
        where
          f = DLNonEmpty.head node
          
-- |Inserting function in BST
insertValue :: Ord x => Tree x -> x -> Tree x
insertValue List a = Node (a :| []) List List
insertValue (Node node left right) a 
    | f == a  = (Node (DLNonEmpty.cons a node) left right)
    | f < a   = (Node node left (insertValue right a))
    | f > a   = (Node node (insertValue left a) right)
        where
          f = DLNonEmpty.head node
        
        
-- |fromList functions in BST
fromListToTree :: Ord x => [x] -> Tree x
fromListToTree list = divideAndGroup (DList.group (DList.sort list))
    where 
      divideAndGroup :: [[x]] -> Tree x
      divideAndGroup [] = List
      divideAndGroup list = Node (fromJust (nonEmpty (DList.last left))) 
                                   (divideAndGroup (DList.init left))
                                     (divideAndGroup right)
          where
            (left, right) = splitAt (((DList.length list) + 1) `div` 2) list


-- |Erasing function in BST
eraseValue :: Ord x => Tree x -> x -> Tree x
eraseValue List a = List
eraseValue (Node node left right) a 
    | f == a  = if DList.length node == 1 
                   then case right of
                         List -> left
                         _    -> Node newNode left newRight
                else (Node (fromJust (nonEmpty (DLNonEmpty.tail node))) left right)
    | f < a   = (Node node left (eraseValue right a))
    | f > a   = (Node node (eraseValue left a) right)
        where
          f = DLNonEmpty.head node
          (newNode, newRight) = newPartTree right
          
          newPartTree :: Tree x -> (NonEmpty x, Tree x)
          newPartTree (Node node List right) = (node, right)
          newPartTree (Node node left right) = (newNode, (Node node newLeft right))
              where
                (newNode, newLeft) = newPartTree left

instance Eq x => Eq (Tree x) where
   (Node nodeF leftF rightF) == (Node nodeS leftS rightS) =
     (nodeF == nodeS) && (leftF == leftS) && (rightF == rightS)
   List == List = True
   _ == _ = False