module Lib
    ( someFunc
    ) where


import           Control.Arrow
import           Control.Monad
import           Data.Foldable       (Foldable, maximumBy)
import           Data.Hashable       (Hashable)
import qualified Data.HashMap.Strict as HM
import           Data.List
import           Data.Matrix
import           Data.Maybe
import           Data.Ord            (comparing)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

maxBy :: (Foldable t, Ord a) => (b -> a) -> t b -> b
maxBy = maximumBy . comparing



countOccurences :: (Hashable a, Eq a) => [a] -> HM.HashMap a Int
countOccurences l = foldl (\m i -> HM.insertWith (+) i 1 m) HM.empty l


size = (8,8)

data Angle = V
           | H
           deriving (Show, Eq)


type Pos = (Int,Int)

data Status = O
            | X
            | F
            deriving (Show, Eq, Read)


data Ship = Ship
          { pos    :: (Int,Int)
          , len    :: Int
          , angle  :: Angle
          , cells  :: [(Int,Int)]
          , values :: [Status]
          , hits   :: Int
          } deriving (Show, Eq)


inField :: (Int,Int) -> Bool
inField (x,y) = x>=0 && x<=mx && y>=0 && y <= my
  where (mx,my) = size


setCells :: Ship -> Maybe Ship
setCells s = if all inField newCells
                then Just $ s { cells = newCells }
                else Nothing
 where newCells = zipWith (\x i -> f x i) adds (replicate (len s) (pos s))
       adds = [(+x) | x <- [0..(len s)-1]]
       f = case angle s of
             V -> second
             H -> first

setValues :: Matrix Status -> Ship -> Maybe Ship
setValues m s = if all (/=O) newValues
                   then Just $ s { values = newValues }
                   else Nothing
  where newValues = map (\(x,y) -> unsafeGet y x m) (cells s)


setHits :: Ship -> Ship
setHits s = s { hits = newHits }
  where newHits = length $ filter (==X) $ values s


newShip :: (Int,Int) -> Int -> Angle -> Ship
newShip p l a = Ship p l a [] [] 0

start = matrix 8 8 $ const F



allShips :: [Int] -> Matrix Status -> [Ship]
allShips lengths m = do
  l <- lengths
  x <- [1..fst size]
  y <- [1..snd size]
  a <- [H,V]
  let s = newShip (x,y) l a
  let s2 = setCells s >>= setValues m >>= return . setHits
  guard (isJust s2)
  return $ fromJust s2


bestShips :: [Ship] -> [Ship]
bestShips s = filter (\i -> hits i == maxHit) s
  where maxHit = maximum $ map hits s




bestPositions :: [Ship] -> [((Int,Int), Int)]
bestPositions ships = HM.toList $ countOccurences unzipped
  where zipped = concat $ map (\s -> zip (cells s) (values s)) ships
        filtered = filter (\(_,v) -> v == F) zipped
        unzipped = map fst filtered



bestPosition :: [((Int,Int),Int)] -> (Int,Int)
bestPosition l = fst $ head $ sortBy (\(_,a) (_,b) -> compare b a) l


getBestPosition :: [Int] -> Matrix Status -> (Int,Int)
getBestPosition l m = bestPosition $ bestPositions $ bestShips $ allShips l m

hit :: (Int,Int) -> [Ship] -> Bool
hit p s = any (==p) $ concat $ map cells s

destroy :: Ship -> Matrix Status -> Matrix Status
destroy s m = foldl (\acc (x,y) -> setElem O (y,x) acc) m $ cells s

destroyed :: Ship -> Bool
destroyed s = all (==X) $ values s

main :: [Ship] -> Matrix Status -> IO ()
main [] _ = putStrLn "WYGRAŁEŚ"
main left m = do
  print m
  let lengths = map len left
  let b = getBestPosition lengths m
  print b
  getLine
  let m2 = if (hit b left)
              then setElem X (snd b, fst b) m
              else setElem O (snd b, fst b) m
  
  let left2 = map (\i -> fromJust $ setCells i >>= setValues m2 >>= return . setHits) left
  let (m3,left3) = foldl (\(matrix,ships) ship ->
                            if destroyed ship
                            then (destroy ship matrix, ships)
                            else (matrix, ship:ships)) (m2,[]) left2
  main left3 m3
  





s = [ newShip (2,3) 5 V
    , newShip (5,1) 4 H
    , newShip (4,3) 3 H
    , newShip (4,5) 2 H
    , newShip (7,6) 3 V
    ]
