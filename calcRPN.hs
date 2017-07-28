import Data.List
import Data.List.Extra (chunksOf)
import Control.Monad
import Control.Applicative (Applicative(..))

type RPN = String



main :: IO ()
main = interact (show . reduceRPN)

reduceRPN :: String -> Double
reduceRPN = head . foldl' f [] . words 
  where f (x:y:ys) "*" = (x * y) : ys
        f (x:y:ys) "+" = (x + y) : ys
        f (x:y:ys) "-" = (x - y) : ys
        f (x:y:ys) "/" = (x / y) : ys
        f (x:y:ys) "^" = (x ** y) : ys
        f (x:y:ys) "**" = (x ** y) : ys
        f (x:xs) "ln" = log x : xs
        f xs "sum" = [sum xs]
        f xs n = read n : xs

-- Hacker: The Art of Exploitation
-- The math example for hacking
-- Use 1,3,4,6 only once with any arithmetics +, -, *, / to get 24.
-- We use brute force first.
-- Reverse poland notation makes constructing the problem space quite easy
-- solvePuzzle [1,3,4,6] "+-*/"
-- Do not assume numbers to be single digit
{-solvePuzzle :: (Num a, Show a) => Double -> String -> [a] -> [RPN]-}
{-solvePuzzle target ops ns = filter ( (target==) . reduceRPN ) allRPNs-}
  {-where allRPNs = map (intersperse ' ') . concat $ [combinePrecedence n p | n <- allNum, p <- allOP]-}
        {-allNum = permutations . concat . intersperse "" . map show $ ns-}
        {-allOP = replicateM ((length ns)-1) ops-}

ns = [1,3,4,6]
ops = ["+", "-", "*", "/"]

combinations k xs = filter ((k==).length) (subsequences xs) -- combinations 3 "+-*/"

{-allNum :: Num a => [a] -> [String]-}
{-allNum = permutations . concat . intersperse "" . map show $ ns-}

{-allOP = [a:b:c:[] | a <- ops, b <- ops, c <- ops] -- use replicateM-}
{-allOPs = combinations ((length ns)-1) ops-}

allOP = replicateM ((length ns)-1) ops
allNum = replicateM 4 . map (show) $ ns

allRPN = [combinePrecedence n p | n <- allNum, p <- allOP]
{-allRPNs = map (intersperse ' ') . concat $ allRPN-}

-- All possible precedence rules for 4 numbers and 3 operations, manually listed out
combinePrecedence :: [String] -> [String] -> [[String]]
combinePrecedence [n1,n2,n3,n4] [p1,p2,p3] = [
   [n1, n2, p1, n3, n4, p2, p3],   -- 2 2 + 2 2 + +
   [n1, n2, n3, n4, p1, p2, p3],   -- 2 2 2 2 + + +
   [n1, n2, p1, n3, p2, n4, p3]]   -- 2 2 + 2 + 2 + 
combinePrecedence ns ps = [ns, ps]


combsWithRep k xs = combsBySize xs !! k
  where
    combsBySize = foldr f ([[]] : repeat [])
    f x = scanl1 $ (++) . map (x :)




-- Try this:
-- solvePath [50, 10, 30, 5, 90, 20, 40, 2, 25, 10, 8]

data Path = Up | Down deriving (Show, Eq, Read)
type Costs a = (a, a)
type Street a = [a]
type Route a = ([Path], Costs a)


-- Using explicit recursion
solvePath :: (Num a, Ord a) => Street a -> [Path]
solvePath xs = reverse . crossStreet (0,0) [] $ 0:xs

crossStreet :: (Num a, Ord a) => Costs a -> [Path] -> Street a -> [Path]
crossStreet (a,b) rd street
  | null street        = rd
  | a > b             = crossStreet (a',b') (Down:rd) street'
  | a == b && a' >= b' = crossStreet (a',b') (Down:rd) street'
  | otherwise          = crossStreet (a',b') (Up:rd) street'
  where (a',b') = (a + min x (y+z), b + min y (x+z))
        (x:y:z:street') = street



-- Using folds
solvePath' = reverse . fst . foldl' crossStreet' ([], (0,0)) . chunksOf 3 . (0:)

crossStreet' :: (Num a, Ord a) => Route a -> Street a -> Route a
crossStreet' (ps, (a,b)) [] = (ps, (a,b))
crossStreet' (ps, (a,b)) [x,y,z] = (ps', (a', b'))
  where (a',b') = (a + min x (y+z), b + min y (x+z))
        ps' = p:ps
        p | a > b || ((a==b) && a' >= b') = Down
          | otherwise                     = Up

-- non Monadic!?
toRoute :: (Num a, Ord a) => Street a -> Route a
toRoute [x,y,z] = ([], (min x (y+z), min y (x+z)))
toRoute _ = ([], (0,0))

data RouteM a = RouteM {getPath :: [Path], getCost :: Costs a} deriving (Show, Eq, Read)

-- Monadic!?
toRouteM :: (Num a, Ord a) => Street a -> RouteM a
toRouteM [x,y,z] = RouteM [] (min x (y+z), min y (x+z))
toRouteM _ = RouteM [] (0, 0)

instance Functor RouteM where -- putting or not putting this `a` here doesn't work
  fmap f r = RouteM (getPath r) (f . fst $ getCost r, f . snd $ getCost r)

instance Applicative RouteM where -- putting or not putting this `a` here doesn't work
  pure x = RouteM [] (x, x)
  f <*> r = undefined
