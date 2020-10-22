inc :: Functor f => f Int -> f Int
inc xs = (+1) <$> xs

data Rank = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | J | Q | K | A
    deriving (Bounded, Enum, Eq, Ord)

-- * Exercise 6

instance Show Rank where
    show R2 = "2"
    show R3 = "3"
    show R4 = "4"
    show R5 = "5"
    show R6 = "6"
    show R7 = "7"
    show R8 = "8"
    show R9 = "9"
    show R10 = "10"
    show J = "J"
    show Q = "Q"
    show K = "K"
    show A = "A"


data Suit = S | H | D | C
    deriving (Bounded, Enum, Eq, Ord, Show)

data Card = Card { rank :: Rank, suit :: Suit }
    deriving (Eq, Ord)

-- * Exercise 7

instance Show Card where
    show (Card { rank = r, suit = s }) = "" ++ show r ++ show s

type Deck = [Card]


fullDeck   = do r <- ["1","2","3"]
                s <- ["A","B","C"]
                return (r ++ s)
                
-- let l = 9; in [(a,b,c) | m <- [0..3*l],
--                          a <- [0..l], b <- [0..l], c <- [0..l],
--                          a + b + c == m ]