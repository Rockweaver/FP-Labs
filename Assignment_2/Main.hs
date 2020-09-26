{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

{-# LANGUAGE TupleSections #-} {- A handy syntax extension. See:

    http://www.haskell.org/ghc/docs/7.6.3/html/users_guide/syntax-extns.html#tuple-sections

-}

module Main where -- Origional name == Assignment2
                         -- Rename to "Main" if you want to compile the game.
                         -- Don't forget to rename it back when submitting!

import Control.Monad

import Data.Char
import Data.List
import Data.Maybe

import System.IO

-- | Rose trees

data Rose a = MkRose a [Rose a]
    deriving (Eq, Show)

-- Exercise 1

root :: Rose a -> a
root (MkRose x _) = x

children :: Rose a -> [Rose a]
children (MkRose _ xs) = xs 

-- Exercise 2

size :: Rose a -> Int
size (MkRose _ []) = 1
size (MkRose _ xs) = 1 + sum(map (size) xs)

leaves :: Rose a -> Int
leaves (MkRose _ []) = 1
leaves (MkRose _ xs) = 0 + sum(map (leaves) xs)

-- | State representation

-- * Players

data Player = P1 | P2
    deriving (Eq, Ord)

instance Show Player where
    show P1 = "Player 1"
    show P2 = "Player 2"
    
-- Exercise 3
    
nextPlayer :: Player -> Player
nextPlayer P1 = P2
nextPlayer P2 = P1

-- * Board

data Field = X | O | B
    deriving (Eq, Ord)

instance Show Field where
    show X = "X"
    show O = "O"
    show B = " "

-- Exercise 4

symbol :: Player -> Field
symbol P1 = X
symbol P2 = O

type Row   = (Field, Field, Field)
type Board = (Row, Row, Row)

-- Exercise 5

transpose3 :: ((a,b,c), (d,e,f), (g,h,i)) -> ((a,d,g), (b,e,h), (c,f,i))
transpose3 ((a,b,c), (d,e,f), (g,h,i)) = ((a,d,g), (b,e,h), (c,f,i))

verticals :: Board -> (Row, Row, Row)
verticals x = transpose3 x

getDiagonals :: ((a,b,c), (d,e,f), (g,h,i)) -> ((a,e,i),(c,e,g))
getDiagonals ((a,b,c), (d,e,f), (g,h,i)) = ((a,e,i),(c,e,g))

diagonals :: Board -> (Row, Row)
diagonals x = getDiagonals x


-- Exercise 6

emptyBoard :: Board
emptyBoard = ((B,B,B),(B,B,B),(B,B,B))

-- Exercise 7
retS :: Field -> String
retS X = "X"
retS O = "O"
retS B = " "

printBoard :: Board -> String
printBoard ((a,b,c),(d,e,f),(g,h,i)) = retS a ++ "|" ++ retS b ++ "|" ++ retS c ++ "\n-+-+-\n" ++
                                       retS d ++ "|" ++ retS e ++ "|" ++ retS f ++ "\n-+-+-\n" ++
                                       retS g ++ "|" ++ retS h ++ "|" ++ retS i ++ "\n"

-- | Move generation
             
-- -- Exercise 8
-- possiblePlaces :: Field -> Board -> [Board]
-- possiblePlaces f ((a,b,c),(d,e,f),(g,h,i)) = 

-- moves :: Player -> Board -> [Board]
-- moves P1 board = possiblePlaces X board
-- moves P2 board = possiblePlaces O board

-- Exercise 8
possMov :: Field -> Board -> Int -> Maybe Board
possMov pIcon ((a,b,c),(d,e,f),(g,h,i)) x | x == 1 && a == B = Just ((pIcon,b,c),(d,e,f),(g,h,i))
                                          | x == 2 && b == B = Just ((a,pIcon,c),(d,e,f),(g,h,i))
                                          | x == 3 && c == B = Just ((a,b,pIcon),(d,e,f),(g,h,i))
                                          | x == 4 && d == B = Just ((a,b,c),(pIcon,e,f),(g,h,i))
                                          | x == 5 && e == B = Just ((a,b,c),(d,pIcon,f),(g,h,i))
                                          | x == 6 && f == B = Just ((a,b,c),(d,e,pIcon),(g,h,i))
                                          | x == 7 && g == B = Just ((a,b,c),(d,e,f),(pIcon,h,i))
                                          | x == 8 && h == B = Just ((a,b,c),(d,e,f),(g,pIcon,i))
                                          | x == 9 && i == B = Just ((a,b,c),(d,e,f),(g,h,pIcon))
                                          | otherwise = Nothing
                                       
moves :: Player -> Board -> [Board]
moves P1 board = [] ++ mapMaybe (possMov X board) [1..9]
moves P2 board = [] ++ mapMaybe (possMov O board) [1..9]

-- | Gametree generation
-- Exercise 9
checkWinner :: Board -> Field -> Bool
checkWinner ((a,b,c),(d,e,f),(g,h,i)) field | a == b && b == c && c == field = True -- abc 
                                            | d == e && e == f && f == field = True -- def
                                            | g == h && h == i && i == field = True -- ghi
                                            | a == d && d == g && g == field = True -- adg
                                            | b == e && e == h && h == field = True -- beh
                                            | c == f && f == i && i == field = True -- cfi
                                            | a == e && e == i && i == field = True -- aei
                                            | c == e && e == g && g == field = True -- ceg
                                            | otherwise = False

hasWinner :: Board -> Maybe Player
hasWinner board | checkWinner board X = Just P1
                | checkWinner board O = Just P2
                | otherwise = Nothing

-- Exercise 10

gameTree :: Player -> Board -> Rose Board
gameTree player board | hasWinner board == Nothing = MkRose board ([] ++ map (gameTree (nextPlayer player))(moves player board))
                      | otherwise = MkRose board []
                    


-- | Game complexity

-- Exercise 11

gameTreeComplexity :: Int
gameTreeComplexity = undefined

-- | Minimax

-- Exercise 12

minimax :: Player -> Rose Board -> Rose Int
minimax = undefined

-- * Lazier minimum and maximums

-- Exercise 13

minimum' :: [Int] -> Int
minimum' = undefined

maximum' :: [Int] -> Int
maximum' = undefined

-- | Gameplay

-- Exercise 14

makeMove :: Player -> Board -> Maybe Board
makeMove = undefined

-- | Main

data PlayerType = Human | Computer

instance Show PlayerType where
    show Human    = "H"
    show Computer = "C"

main :: IO ()
main = do
    typeOfP1 <- askFor "Should Player 1 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]
    typeOfP2 <- askFor "Should Player 2 be a (H)uman or a (C)omputer player?"
                       [Human, Computer]

    let playerType :: Player -> PlayerType 
        playerType P1 = typeOfP1
        playerType P2 = typeOfP2

        gameLoop :: Player -> Board -> IO ()
        gameLoop p b = do
            putStrLn ("\n" ++ printBoard b)
            case hasWinner b of
                Just p  -> putStrLn (show p ++ " has won!")
                Nothing -> do
                    putStr   ("It's " ++ show p ++ "'s turn. ")
                    mb' <- case playerType p of
                        Human    -> humanMove    p b
                        Computer -> computerMove p b
                    case mb' of
                        Nothing -> do putStr   "No more moves are possible. "
                                      putStrLn "It's a draw."
                        Just b' -> gameLoop (nextPlayer p) b'

        humanMove :: Player -> Board -> IO (Maybe Board)
        humanMove p b =
            case moves p b of
              [] -> return Nothing
              possibleMoves -> do
                putStrLn "Possible moves are:"
                putStrLn (listMoves possibleMoves)
                i <- askFor "Make your choice:" [1..length possibleMoves]
                return (Just (possibleMoves !! (i-1)))

        computerMove :: Player -> Board -> IO (Maybe Board)
        computerMove p b = do
            putStrLn "Thinking..."
            return (makeMove p b)

        listMoves :: [Board] -> String
        listMoves = intercalate "\n"
                    . map (intercalate "    ")
                    . transpose
                    . map lines
                    . map (\(i,b) -> "(" ++ show i ++ "): \n" ++ printBoard b) 
                    . zip [1 :: Integer ..]

    gameLoop P1 emptyBoard

askFor :: Show a => String -> [a] -> IO a
askFor m xs = do
    putStr (m ++ " ")
    hFlush stdout
    i <- getLine
    case find ((map toLower i ==) . map toLower . show) xs of
        Nothing -> do putStrLn $ "I didn't understand you. Enter one of: "
                                 ++ intercalate ", " (map show xs) ++ "."
                      askFor m xs
        Just y  -> return y