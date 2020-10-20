greet timeOfDay name = "Good " ++ timeOfDay ++ " " ++ name ++ "!!!"

-- Write a function noOfSol that, for some a, b, and c, determines the number of solutions of the equation ax2 + bx + c = 0, using case distinction.
checkNoOfSol :: Int -> Int
checkNoOfSol x | x < 0  = 0
               | x == 0 = 1
               | x > 0  = 2

noOfSol :: Int -> Int -> Int -> Int
noOfSol a b c = checkNoOfSol (fromIntegral ((b^2) - (4 * a * c)))

-- Write a function pow2 :: Int -> Int that takes an Int n computes 2^n using direct recursion.
powfunc :: Int -> Int -> Int -> Int
powfunc x _ 0 = 1
powfunc x _ 1 = x
powfunc x xo n = powfunc (x*xo) xo (n-1) 

pow2 :: Int -> Int
pow2 n = powfunc 2 2 n

-- Write a recursive function pow that takes two Ints, x and n, and computes xn.

pow x n = powfunc x x n

-- Write the following functions using direct recursion. If no type signature is given, also give the type of the function.

-- The product function computes the product of a finite list of numbers.
multWithNext :: Int -> [Int] -> [Int]
multWithNext x (y:ys) = [(x*y)] ++ ys

productS :: [Int] -> Int
productS (x:xs) | xs == [] = x
                | otherwise = productS (multWithNext x xs)

intersperse :: Char -> [Char] -> String
intersperse x (xs:xss) | xss == [] = [xs]
                       | otherwise = [xs] ++ [x] ++ (intersperse x xss)

unlinesS :: [String] -> String
unlinesS (x:xs) | xs == [] = x
                | otherwise = "" ++ x ++ "|" ++ unlinesS xs

insertEverywhere :: a -> [a] -> [[a]]
insertEverywhere x []        = [[x]]
insertEverywhere x xs@(y:ys) = (x:xs) : map (y:) (insertEverywhere x ys)
