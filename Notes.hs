greet timeOfDay name = "Good " ++ timeOfDay ++ " " ++ name ++ "!!!"

sum' :: [Int] -> Int
sum' (x:xs) = x + sum' xs
sum' [ ] = 0

fizzbuzz (m,n) xs = ([x | x <- xs, x `mod` m==0]
                    ,[x | x <- xs, x `mod` n==0, x `mod` m/=0]
                    ,[x | x <- xs, x `mod` n/=0, x `mod` m/=0]
                    )

-- fizzbuzz2 (m,n) [] = ([],[],[])
-- fizzbuzz2 (m,n) (x:xs) = 
    
-- (ms,ns,os) = fizzbuzz2 (m,n) xs

fac 0 = 1
fac n = n * fac (n-1)

--type declaration (Like enum)
data Direction = North
                | South
                | East
                | West

directionName North = "N"
directionName East = "E"
directionName South = "S"
directionName West = "W"

--Point example
data Point = Pt Float Float
:t Pt 2.0 3.0

norm :: Point -> Float
norm (Pt x y) = sqrt (x*x + y*y)

zipPoint :: [Float] -> [Float] -> [Point]

-- uncurry
uncurry' ::
uncurry' f (x,y) = x+y
uncurry' = \f 

--Shape declaration
data Shape = Rectangle Point Float Float
            | Circle Point Float
            | Triangle Point Point Point
-- Pattern Match
perimeter :: Shape -> Float
perimeter (Rectangle p w h) = 2 * w + 2 * h
perimeter (Circle p r) = 2 * pi * r
perimeter (Triangle a b c) = distance a b + distance b c + distance c a where
    distance (Pt x1 y1) (Pt x2 y2) = sqrt ((x1-x2)^2 + (y1-y2)^2)

-- In an object oriented language it is really easy to add a new object
-- In a functional oriented language de declaration needs to be changed and a case needs to be added in the patternmatch
-- Add and new method it is easier in a functional oriended language
-- expressionproblem on google for extra info

data Point = Pt Float Float
data Vector = Vec Float Float
-- to create a difference between a point and a direction

-- define your own lists
data IntList
    = EmptyList | Cons Int IntList
data IntTree
    = EmptyTree | Node Int IntTree IntTree -- a left and a right subtree

elemList :: Int -> IntList -> Bool
elemList x 

elemTree :: Int -> IntTree -> Bool
elemTree _ EmptyTree = False
elemTree j (Node i lt rt) = j == i || elemTree j lt || elemTree j rt

treeHeight :: IntTree -> Int
treeHeight EmptyTree = 0
treeHeight (Node _ lt rt) = 1 + max (treeHeight lt) (treeHeight rt)

treeSize :: IntTree -> Int
treeSize EmptyTree = 0
treeSize (Node _ lt rt) = 1 + treeSize lt + treeSize rt


almostTreeFold op EmptyTree = 0
almostTreeFold op (Node _ lt rt) = 1 + op (almostTreeFold op lt) (almostTreeFold op rt)

find :: (a -> Bool) -> [a] -> Maybe a
find p [] = Nothing
find p (x : xs) | p x = Just x
                | otherwise = find p xs

class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

data FunnyPair = FP Int Int -- data could be replaced by newtype is the same

instance Eq FunnyPair where
    FP x y == FP uv = x * v == y * u

newtype FunnyPair = FP Int Int  
type otherpair = OP Int Int 

--Tree equality check
data Tree a = EmptyTree
            | Node a (Tree a) (Tree a)

instance Eq a => Eq (Tree a) where
    EmptyTree == EmptyTree = True
    (Node i lt rt) == (Node j lt2 rt2) = i == j && lt == lt2 && rt == rt2
    _ == _ = false

-- define your own datatypes, it is easy and really easy to use/read
data Status = Stopped | Running

