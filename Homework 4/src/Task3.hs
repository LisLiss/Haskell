module Task3 where

import Data.Hashable
import Data.Vector
import Control.Concurrent.STM
import Control.Monad as CM
import Control.Exception.Base

data CHashTable k v = CHashTable
                      (TVar Int)
                      (TVar (Data.Vector.Vector (TVar [(k, v)])))

constSize :: Int
constSize = 12

getCHT  :: (Hashable k) => (Eq k) => k -> CHashTable k v -> IO (Maybe v)
getCHT index table@(CHashTable _s _v) = mask_ $
                       atomically $ do
                          let hashed = hash index
                          vector <- readTVar _v
                          let needed = vector ! mod hashed (Data.Vector.length vector)
                          position <- readTVar needed
                          return (lookup index position)

putCHT  :: (Hashable k) => (Eq k) => k -> v -> CHashTable k v -> IO ()
putCHT index storing (CHashTable _s _v) = mask_ $
                                         atomically $ do
                                            let hashed = hash index
                                            vector <- readTVar _v
                                            let needed = vector ! mod hashed (Data.Vector.length vector)
                                            position <- readTVar needed
                                            let len = Data.Vector.length vector
                                            case lookup index position of
                                               Just _ -> writeTVar needed (update storing index position)
                                               _ ->  do
                                                    size <- readTVar _s
                                                    let coef = countCoef size (Data.Vector.length vector)
                                                    if coef > 0.5 then do
                                                       _v2 <- Data.Vector.replicateM (len * 2) (newTVar [])
                                                       lst <- CM.forM (Data.Vector.toList vector) readTVar
                                                       CM.forM_ ((index, storing) : Prelude.concat lst) (\(c, d) -> do
                                                                   let hashed2 = hash c
                                                                   let needed2 = _v2 ! mod hashed (Data.Vector.length _v2)
                                                                   position2 <- readTVar needed2
                                                                   writeTVar needed2 ((c, d) : position2)
                                                                 )
                                                       writeTVar _v _v2
                                                    else do
                                                      let newP = (index, storing) : position
                                                      writeTVar needed newP
                                                    writeTVar _s (size + 1)
        where
          update elem key = Prelude.map (\(key1, elem1) -> if key1 == key then
                                                       (key, elem) else (key1, elem1))
          countCoef a b = fromIntegral a / fromIntegral b

newCHT  :: IO (CHashTable k v)
newCHT = mask_  $
            atomically $ do
               let temp = Data.Vector.replicateM constSize (newTVar [])
               let binded = temp >>= newTVar
               newArray <-  binded
               newSize <- newTVar 0
               return $ CHashTable newSize newArray

sizeCHT :: CHashTable k v -> IO Int
sizeCHT (CHashTable _s _v) = mask_ (readTVarIO _s)