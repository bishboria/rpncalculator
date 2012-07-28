import Data.Char

-- usage:
-- rpn ["1","1","+"] []
-- ["2"]
-- 
-- rpn ["1","10","10","*","+"]
-- ["101"]

rpn :: [String] -> [String] -> [String]
rpn []     stack   = stack
rpn (x:xs) stack
    | isANumber x  = rpn xs (x:stack)
    | otherwise    = rpn xs $ apply x stack

apply :: String -> [String] -> [String]
apply _  []       = []
apply _ [a]       = [a]
apply op (a:b:xs)
    | binary op   = (show $ operator op (read a) (read b)) : xs
    | otherwise   = []

operator :: Num a => String -> (a -> a -> a)
operator op
    | op == "+" = (+)
    | op == "*" = (*)

binary :: String -> Bool
binary op = op /= "!"

isANumber :: String -> Bool
isANumber xs =
    and [isNumber x | x <- xs]
