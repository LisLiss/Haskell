{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module Task8 where


import Control.Comonad
import System.Random
import Control.Monad (forM, forM_)
import System.Process     (callCommand)
import Control.Concurrent (threadDelay)
import System.Console.ANSI

data ListZipper a = LZ [a] a [a]  -- allows to focus on a single element

listLeft, listRight :: ListZipper a -> ListZipper a
listLeft  (LZ (a:as) x bs) = LZ as a (x:bs)
listLeft a = a

listRight (LZ as x (b:bs)) = LZ (x:as) b bs
listRight a = a

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

toList :: Int -> ListZipper a -> [a]
toList n (LZ ls x rs) = reverse (take n ls) ++ [x] ++ take n rs

toList2 :: Int -> Grid a -> [[a]]
toList2 n = fmap (toList n) . toList n . unGrid

instance Functor ListZipper where
    fmap f (LZ ls x rs) = LZ (map f ls) (f x) (map f rs)

instance Comonad ListZipper where
  extract (LZ _ x _) = x
  duplicate = genericMove listLeft listRight

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

genericMove :: (z a -> z a)
            -> (z a -> z a)
            -> z a
            -> ListZipper (z a)
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)


newtype Grid a = Grid { unGrid :: ListZipper (ListZipper a) }  -- 2D grid

up, down :: Grid a -> Grid a
up   (Grid g) = Grid (listLeft  g)
down (Grid g) = Grid (listRight g)

left, right :: Grid a -> Grid a
left  (Grid g) = Grid (fmap listLeft  g)
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right
vertical   = genericMove up   down

instance Comonad Grid where
    extract :: Grid a -> a
    extract = gridRead

    duplicate :: Grid a -> Grid (Grid a)
    duplicate = Grid . fmap horizontal . vertical

instance Functor Grid where
  fmap f (Grid a) = Grid $ (f <$>) <$> a

data Cell = Cell Typ Int StdGen

instance Show Cell where
  show (Cell Active _ _) = "#"
  show (Cell Latent _ _) = "%"
  show (Cell Immune _ _) = "@"
  show (Cell Without _ _) = " "

data Typ = Active | Latent | Immune | Without

activeCount :: [Cell] -> Int
activeCount = length . filter (\case
                                   Cell Active _ _ -> True
                                   Cell Latent _ _ -> True
                                   _ -> False)

activeLst :: [Cell] -> [Cell]
activeLst = filter (\case
                        Cell Active _ _ -> True
                        Cell Latent _ _ -> True
                        _ -> False)

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals
  where horizontals = [left, right]
        verticals   = [up, down]

activeNeighbours :: Grid Cell -> Int
activeNeighbours g = activeCount
                  $ map (\direction -> extract $ direction g) neighbours

activeLstNeighbours :: Grid Cell -> [Cell]
activeLstNeighbours g = activeLst
                  $ map (\direction -> extract $ direction g) neighbours

evolve :: (Int, Int, Int, Float) -> Grid Cell -> Grid Cell
evolve config = extend (rule config)

rule :: (Int, Int, Int, Float) -> Grid Cell -> Cell
rule (dA, dL, dI, prob) g =
    if activeNeighbours g == 0 then
            case extract g of
                  Cell Active 1 r -> Cell Immune dI (snd(randomR (0, 1 :: Float) r))
                  Cell Immune 1 r -> Cell Without 1 (snd(randomR (0, 1 :: Float) r))
                  Cell Latent 1 r -> Cell Active dA (snd(randomR (0, 1 :: Float) r))
                  Cell Active k r -> Cell Active (k - 1) (snd(randomR (0, 1 :: Float) r))
                  Cell Immune k r -> Cell Immune (k - 1) (snd(randomR (0, 1 :: Float) r))
                  Cell Latent k r -> Cell Latent (k - 1) (snd(randomR (0, 1 :: Float) r))
                  Cell Without k r -> Cell Without k (snd(randomR (0, 1 :: Float) r))
    else
            case extract g of
                  Cell Active 1 r -> Cell Immune dI (snd(randomR (0, 1 :: Float) r))
                  Cell Immune 1 r -> Cell Without 1 (snd(randomR (0, 1 :: Float) r))
                  Cell Latent 1 r -> Cell Active dA (snd(randomR (0, 1 :: Float) r))
                  Cell Active k r -> Cell Active (k - 1) (snd(randomR (0, 1 :: Float) r))
                  Cell Immune k r -> Cell Immune (k - 1) (snd(randomR (0, 1 :: Float) r))
                  Cell Latent k r -> Cell Latent (k - 1) (snd(randomR (0, 1 :: Float) r))
                  Cell Without k r -> attack (snd(randomR (0, 1 :: Float) r))
  where
     attack r1 = do
        let checked = map (\(Cell _ _ generator) ->
                    prob > fst(randomR (0, 1 :: Float) generator))
                    (activeLstNeighbours g)
        if notElem True checked then Cell Without 0 r1
          else Cell Latent dL r1

startGame :: (Int, Int, Int, Float) -> Int -> Int -> Grid Cell -> IO()
startGame config iterations size grid =
      if iterations == 0 then putStrLn ""
      else do
         threadDelay 200000
         callCommand "tput reset"
         Task8.print size grid
         startGame config (iterations - 1) size (evolve config grid)

startTable :: (Int, Int, Int, Float) -> Int -> IO (Grid Cell)
startTable (dA, dL, dI, prob) size = do
   first <- forM [1..(div size 2)] (const makeLst)
   second <- forM [1..(div size 2)] (const makeLst)
   mid <- makeLstMid
   return (Grid (LZ first mid second))
  where
     allLst = do
       left1 <- forM [1..(div size 2)] (\elem1 -> Cell Without 0 <$> newStdGen)
       right1 <- forM [1..(div size 2)] (\elem1 -> Cell Without 0 <$> newStdGen)
       return (left1, right1)
     makeLst = do
         (left1, right1) <- allLst
         gener <- newStdGen
         return (LZ left1 (Cell Without 0 gener) right1)
     makeLstMid = do
         (left1, right1) <- allLst
         gener <- newStdGen
         return (LZ left1 (Cell Latent dL gener) right1)

print :: Show a => Int -> Grid a -> IO ()
print count g = do
  setSGR [SetColor Foreground Dull Red]
  putStrLn "Red - With COVID"
  setSGR [SetColor Foreground Dull Yellow]
  putStrLn "Yellow- Latent COVID"
  setSGR [SetColor Foreground Dull Green]
  putStrLn "Green - With Immune"
  forM_ (map (map show) (toList2 (div (count - 1) 2) g))
         (\lst -> do
                  forM_ lst (\struct -> do
                               case struct of
                                 "#" -> setSGR [SetColor Foreground Dull Red]
                                 "%" -> setSGR [SetColor Foreground Dull Yellow]
                                 _ -> setSGR [SetColor Foreground Dull Green]
                               putStr struct
                               putStr " ")
                  putStrLn "")


play :: (Int, Int, Int, Float) -> Int -> Int -> IO()
play config iterations size1 = do
     let size = (div size1 2) * 2 + 1 --to make it odd
     startGrid <- startTable config size
     startGame config iterations size startGrid

modelCOVID :: IO()
modelCOVID = do
   --dA, dL, dI, prob
   --dA = count of days with COVID before treatment
   --dL = count of days with COVID without symptoms
   --dI = count of days with immune after COVID
   --prob - probability to catch COVID from neighbours
   play (2, 1, 1, 0.45) 30 21
   setSGR [SetColor Foreground Vivid Magenta]