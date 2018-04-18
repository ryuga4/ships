module Lib where


import           Control.Arrow
import           Control.Monad
import           Control.Monad.ST
import           Data.Array.ST
import           Data.Foldable       (Foldable, maximumBy)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.List.Unique
import           Data.Matrix
import           Data.Maybe
import           Data.Ord            (comparing)
import           Data.STRef
import           System.Random
import Control.Monad.Trans.List
import Control.Monad.Trans.Class


maxBy :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maxBy = maximumBy . comparing



countOccurences :: (Hashable a, Eq a) => [a] -> HM.HashMap a Int
countOccurences l = foldl (\m i -> HM.insertWith (+) i 1 m) HM.empty l


justToList (Just x) = x
justToList Nothing  = []


shuffle :: [a] -> StdGen -> ([a],StdGen)
shuffle xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs





data Angle = V
           | H
           deriving (Show, Eq)


type Pos = (Int,Int)

data Status = O
            | X
            | F
            | D
            deriving (Eq, Read)



colliding :: Status -> (Int,Int) -> [(Int,Int)]
colliding F _ = []
colliding X (x,y) = [(x-1,y-1),(x-1,y+1),(x+1,y-1),(x+1,y+1)]
colliding O (x,y) = [(x,y)]
colliding D (x,y) = colliding X (x,y) ++ [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]



instance Show Status where
  show O = "O"
  show X = "X"
  show F = "_"
  show D = "*"


data Ship = Ship
          { pos    :: (Int,Int)
          , len    :: Int
          , angle  :: Angle
          , cells  :: [(Int,Int)]
          , values :: [Status]
          , hits   :: Int
          } deriving (Show, Eq)


inField :: (Int,Int) -> (Int,Int) -> Bool
inField size (x,y) = x>=1 && x<=mx && y>=1 && y <= my
  where (mx,my) = size


setCells :: (Int,Int) -> Ship -> Maybe Ship
setCells size s = if all (inField size) newCells
                then Just $ s { cells = newCells }
                else Nothing
 where newCells = zipWith (\x i -> f x i) adds (replicate (len s) (pos s))
       adds = [(+x) | x <- [0..(len s)-1]]
       f = case angle s of
             V -> second
             H -> first

setValues :: (Int,Int) -> Matrix Status -> Ship -> Maybe Ship
setValues size m s = if collisionsD || collisionsO || collisionsX
                   then Nothing
                   else Just $ s { values = newValues }
  where
    collisions f = any (==f)
      $ map (\(x,y) -> unsafeGet y x m)
      $ filter (inField size)
      $ concat
      $ map (colliding f)
      $ cells s

    collisionsD = collisions D
    collisionsO = collisions O
    collisionsX = collisions X
    newValues = map (\(x,y) -> unsafeGet y x m) (cells s)


setHits :: Ship -> Ship
setHits s = s { hits = newHits }
  where newHits = length $ filter (==X) $ values s


newShip :: (Int,Int) -> Int -> Angle -> Ship
newShip p l a = Ship p l a [] [] 0

start size = matrix (fst size) (snd size) $ const F



allShips :: (Int,Int) -> [Int] -> Matrix Status -> [Ship]
allShips size lengths m = do
  l <- lengths
  x <- [1..fst size]
  y <- [1..snd size]
  a <- [H,V]
  let s = newShip (x,y) l a
  let s2 = setCells size s >>= setValues size m >>= return . setHits
  guard (isJust s2)
  return $ fromJust s2


bestShips :: [Ship] -> [Ship]
bestShips s = bestShips' s maxHit
  where maxHit = maximum $ map hits s
        bestShips' s n = if x == [] then bestShips' s (n-1) else x
          where x = filter (\i -> hits i /= len i && hits i == n) s



bestPositions :: [Ship] -> [((Int,Int), Int)]
bestPositions ships = HM.toList $ countOccurences unzipped
  where zipped = concat $ map (\s -> zip (cells s) (values s)) ships
        filtered = filter (\(_,v) -> v == F) zipped
        unzipped = map fst filtered



bestPosition :: Int -> [((Int,Int),Int)] -> (Int,Int)
bestPosition num l = fst $ allOptions !! rand
  where bestCount = maximum $ map snd l
        allOptions = filter (\i -> snd i == bestCount) l
        rand = mod num $ length allOptions



getBestPosition :: (Int,Int) -> Int -> [Int] -> Matrix Status -> (Int,Int)
getBestPosition size num l m = bestPosition num $ bestPositions $ bestShips $ allShips size l m

hit :: (Int,Int) -> [Ship] -> Bool
hit p s = any (==p) $ concat $ map cells s

destroy :: Ship -> Matrix Status -> Matrix Status
destroy s m = foldl (\acc (x,y) -> setElem D (y,x) acc) m $ cells s

destroyed :: Ship -> Bool
destroyed s = all (==X) $ values s





