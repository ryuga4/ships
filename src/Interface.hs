module Interface where



import System.Random
import Lib
import Data.Matrix
import Data.Maybe
import Data.List.Unique
import Control.Monad




interface :: Monad m
          => Settings m c a
          -> m (Int, Matrix Status, StdGen)
interface (Settings genM sizeM cM printM lengthsM getM checkM dUpdateM dmUpdateM endM) = do
  gen <- genM
  size <- sizeM
  c <- cM
  mainM gen size c (start size) 0 printM lengthsM getM checkM dUpdateM dmUpdateM endM
  where
    mainM :: Monad m
          => StdGen -- stdGen
          -> (Int,Int) -- size
          -> c -- custom data c
          -> Matrix Status -- matrix
          -> Int -- counter
          -> (Matrix Status -> m a) -- output matrix
          -> (c -> m [Int]) -- get lengths
          -> ((Int,Int) -> [Int] -> Matrix Status -> Int -> m (Int,Int)) -- position  input
          -> ((Int,Int) -> c -> m Status) -- position check
          -> (c -> Matrix Status -> m c) -- data update
          -> (c -> Matrix Status -> m (Matrix Status, c)) -- data and matrix update
          -> (c -> m Bool) -- end function
          -> m (Int, Matrix Status,  StdGen)
    mainM gen size c matrix x printM lengthsM getM checkM dUpdateM dmUpdateM endM = do
      e <- endM c
      if e
        then stop
        else go
      where
        stop = printM matrix >> return (x,matrix,gen)
        go = do
          let (num, gen2) = next gen
          lengths <- lengthsM c
          printM matrix
          b <- getM size lengths matrix num
          st <- checkM b c

          let m2 = setElem st (snd b, fst b) matrix

          c2 <- dUpdateM c m2
          (m3,c3) <- dmUpdateM c2 m2
          mainM gen2 size c3 m3 (x+1) printM lengthsM getM checkM dUpdateM dmUpdateM endM

-- map len
--  
--  




data Settings m c a = Settings
                { genM :: (m StdGen) -- stdGen
                , sizeM ::  (m (Int,Int)) -- size
                , cM :: (m c) -- custom data c 
                , printM ::  (Matrix Status -> m a) -- output matrix
                , lengthsM :: (c -> m [Int]) -- get lengths
                , getM :: ((Int,Int) -> [Int] -> Matrix Status -> Int -> m (Int,Int)) -- position input
                , checkM :: ((Int,Int) -> c -> m Status) -- position check
                , dUpdateM :: (c -> Matrix Status -> m c) -- data update
                , dmUpdateM :: (c -> Matrix Status -> m (Matrix Status, c)) -- data and matrix update
                , endM :: (c -> m Bool) -- end function
                }





defaultSettings :: (Int,Int) -> [Ship] -> Settings IO [Ship] ()
defaultSettings size ships = do
   Settings newStdGen (return size) (return ships) printM lengthsM getM checkM dUpdateM dmUpdateM endM 

  where
    printM :: Matrix Status -> IO ()
    printM = print

    getM :: (Int,Int) -> [Int] -> Matrix Status -> Int -> IO (Int,Int)
    getM size lengths m num = print pos >> getLine >> return pos
      where pos = getBestPosition size num lengths m

    checkM :: (Int,Int) -> [Ship] -> IO Status
    checkM pos left = return $ if (hit pos left) then X else O

    lengthsM c = return $ map len c

    dUpdateM c m = return $ map (\i -> fromJust $ setCells size i >>= setValues size m >>= return . setHits) c

    dmUpdateM c m = return $ foldl (\(matrix,ships) ship ->
                        if destroyed ship
                         then (destroy ship matrix, ships)
                         else (matrix, ship:ships)) (m,[]) c

    endM c = return $ c == []



main :: (Int,Int) -> [Ship] -> IO (Int, Matrix Status, StdGen)
main size ships = interface $ defaultSettings size ships



