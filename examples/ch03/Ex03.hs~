module Ex03 where

import Data.List

myLen :: [t] -> Int
myLen (_:xs) = 1+myLen(xs)
myLen [] = 0

mySum [] = 0
mySum (x:xs) = x + mySum(xs)

myMean [] = 0
myMean xs = fromIntegral(mySum(xs))/fromIntegral(myLen(xs))


palindrome [] = []
palindrome (xs) = xs ++ myFlip (xs)
        where myFlip [] = []
              myFlip (x:xs) = myFlip(xs) ++ x:[]

--palindrome [] = []
--palindrome (x:xs) = x ++ palindrome(xs) ++ x

isPalindrome [] = True
isPalindrome (l) = head(l) == last(l) && isPalindrome(middle l)
        where middle l = drop 1 (init l)

sortList l = sortBy compareLength l
        where compareLength a b = compare (length a) (length b)

myinterperse :: a -> [[a]] -> [a]
myinterperse _ [x] = x
myinterperse a [] = []
myinterperse a l = head l ++ a:[] ++ next
        where next = myinterperse a (drop 1 l)

--myIntersperse _ [] = []
--myIntersperse _ [x] = x
--myIntersperse a (x:xs) = x ++ [a] ++ myIntersperse a xs
