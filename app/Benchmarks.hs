module Benchmarks where


import System.Random
import Lib
import Data.Matrix
import Data.Maybe
import Data.List.Unique
import Control.Monad



main2 :: (Int,Int) -> [Ship] -> Matrix Status -> StdGen -> Integer
main2 _ [] _ _ = 0
main2 size left m gen = do
       let (num, gen2) = next gen
       let lengths = map len left
       let b = getBestPosition size num lengths m
       let m2 = if (hit b left)
                then setElem X (snd b, fst b) m
                else setElem O (snd b, fst b) m

       let left2 = map (\i -> fromJust $ setCells size i >>= setValues size m2 >>= return . setHits) left
       let (m3,left3) = foldl (\(matrix,ships) ship ->
                            if destroyed ship
                            then (destroy ship matrix, ships)
                            else (matrix, ship:ships)) (m2,[]) left2
       1 + (main2 size left3 m3 gen2)




s = [ newShip (1,1) 2 V
    , newShip (1,4) 5 V
    , newShip (3,2) 3 H
    , newShip (7,1) 3 V
    , newShip (3,6) 4 H
    ]




randomNth :: [a] -> StdGen -> (a, StdGen)
randomNth [] _ = error "Empty list"
randomNth l gen = (l !! mod n (length l), gen2)
 where (n,gen2) = next gen


testShips :: (Int,Int) -> Int -> Int -> [Ship] -> StdGen -> ([Ship], StdGen)
testShips size l 0 acc gen = (acc,gen)
testShips size l n acc gen = if isJust s2
                        then testShips size l (n-1) (fromJust s2 : acc) gen4
                        else testShips size l n acc gen4
  where (x,gen2) = randomNth [1..fst size] gen
        (y,gen3) = randomNth [1..snd size] gen2
        (a,gen4) = randomNth [H,V] gen3
        s = newShip (x,y) l a
        s2 = setCells size s





testShips2 :: (Int,Int) -> StdGen -> [[Ship]]
testShips2 size gen = do
  let (gen2,gen3) = split gen
  let (gen4,gen5) = split gen2
  s1 <- fst $ testShips size 5 10 [] gen
  s2 <- fst $ testShips size 4 10 [] gen2
  s3 <- fst $ testShips size 3 10 [] gen3
  s4 <- fst $ testShips size 3 10 [] gen4
  s5 <- fst $ testShips size 2 10 [] gen5
  let fleet = [s1,s2,s3,s4,s5]
  let allCells = concat $ map cells fleet
  guard (allUnique allCells)
  let mapped = sequence $ map (\i -> setValues size (start size) i >>= return . setHits) fleet
  guard (isJust mapped)
  return $ fromJust mapped



testShips3 :: (Int,Int) -> [[Ship]] -> StdGen -> Integer -> Integer -> IO Double
testShips3 _ [] _ n sum = return $ fromInteger sum / fromInteger n
testShips3 size (fleet:rest) gen n sum = do
  --if n > 0
  --  then print $ show sum ++ " / " ++ show n ++ " = " ++ show (fromInteger sum / fromInteger n)
  --  else return ()
  let (gen2,gen3) = split gen
  let time = main2 size fleet (start size) gen2
  testShips3 size rest gen3 (n+1) (sum+time)



testShips4 :: Int -> IO ()
testShips4 n = do
  let size = (n,n)
  gen <- newStdGen
  let fleets = take 100 $ testShips2 size gen
  avg <- testShips3 size fleets gen 0 0
  print $ show n ++ "x" ++ show n ++ "   " ++ show avg ++ "   " ++ show (100 * avg / (fromIntegral n * fromIntegral n)) ++ "%"
  testShips4 (n+1)

