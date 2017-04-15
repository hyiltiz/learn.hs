import Data.List
import Control.Monad

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


data Path = Up | Down deriving (Show, Eq, Read)
type Costs a = (a, a)
type Street a = [a]

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
