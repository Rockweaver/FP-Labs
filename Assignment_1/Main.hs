module Main where

{- This is a framework in which all functions to be written are "undefined".  -
 - Note that in most cases parameters, pattern-matching and guards have been  -
 - omitted! You will have to add those yourself.                              -}

import Data.Char
import Data.List
import Data.Maybe

-- | Model

type Field = String
type Row   = [Field]
type Table = [Row]

-- | Main

main :: IO ()
main = interact (unlines . exercise . lines)

exercise :: [String] -> [String]
exercise = printTable
         . project ["last", "first", "salary"]
         . select "gender" "male"
         . parseTable

-- | Parsing

-- * Exercise 1

parseTable :: [String] -> Table
parseTable listOfStrings = map words listOfStrings

-- | Printing

-- * Exercise 2

printLine :: [Int] -> String
printLine ints = create ints "+"
    where
        create :: [Int] -> String -> String
        create [] out = out
        create (x:xs) out = "+" ++ (concat $ replicate x "-") ++ create xs out

-- * Exercise 3

printField :: Int -> String -> String
printField x string | (all isDigit string) == False   = string ++ replicate (x - (length string)) ' '
                    | (all isDigit string) == True    = replicate (x - (length string)) ' ' ++ string
-- * Exercise 4

            
printRow :: [(Int, String)] -> String
printRow xs = "|" ++ intercalate "|" (map (uncurry printField) xs) ++ "|"  

-- * Exercise 5

columnWidths :: Table -> [Int]
columnWidths table = map maximum (map (map length) (transpose table))

-- * Exercise 6

printTable :: Table -> [String]
printTable table@(header:rows)
    = [printLine (columnWidths table)] ++
    [printRow (zip (columnWidths table)(map (map toUpper) header))] ++
    [printLine (columnWidths table)] ++
    map printRow (map (zip (columnWidths table)) rows ) ++
    [printLine (columnWidths table)]

-- | Querying

-- * Exercise 7

select :: Field -> Field -> Table -> Table
select column value table@(header:rows)
    = search (maybe "" show (elemIndex column header)) rows "" [[]]
        where
            search = undefined


-- * Exercise 8

project :: [Field] -> Table -> Table
project columns table@(header:_)
    =  transpose (map ((transpose table) !!) (mapMaybe(`elemIndex` header) columns))