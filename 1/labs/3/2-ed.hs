import Data.List (nub)

data Suit = Spades | Hearts | Diamonds | Clubs deriving (Ord, Eq, Show)

data Rank = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace deriving (Ord, Eq)

instance Show Rank where
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"
  show Six = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine = "9"
  show Ten = "10"
  show Jack = "J"
  show Queen = "Q"
  show King = "K"
  show Ace = "A"

data Card = Card Suit Rank

instance Show Card where
  show (Card s r) = show r ++ " of " ++ show s

isMinor :: Card -> Bool
isMinor (Card _ r) = r >= Two && r <= Ten

sameSuit :: Card -> Card -> Bool
sameSuit (Card s1 _) (Card s2 _) = s1 == s2

beats :: Card -> Card -> Bool
beats (Card _ r1) (Card _ r2) = r1 > r2

beatsTrump :: Card -> Card -> Suit -> Bool
beatsTrump (Card s1 r1) (Card s2 r2) trump =
  if s1 == trump && s2 /= trump
    then True
    else if s1 /= trump && s2 == trump
      then False
      else r1 > r2

beatsList :: [Card] -> Card -> Suit -> [Card]
beatsList cs c trump = filter (\c' -> beatsTrump c' c trump) cs

cardValue :: Rank -> Int
cardValue Two = 2
cardValue Three = 3
cardValue Four = 4
cardValue Five = 5
cardValue Six = 6
cardValue Seven = 7
cardValue Eight = 8
cardValue Nine = 9
cardValue Ten = 10
cardValue Jack = 10
cardValue Queen = 10
cardValue King = 10
cardValue Ace = 11

possibleValues :: [Card] -> [Int]
possibleValues cs =
  let possibleAces = map (\(Card _ r) -> if r == Ace then [1, 11] else [cardValue r]) cs
      allCombinations = foldr (\xs acc -> [x + y | x <- xs, y <- acc]) [0] possibleAces
  in nub allCombinations

main :: IO ()
main = do
  let cards = [Card Spades Two, Card Diamonds Four, Card Clubs Jack, Card Hearts Ace]
  let trump = Spades
  putStrLn "Cards:"
  print cards
  putStrLn "Possible values:"
  print $ possibleValues cards
  putStrLn "Beats list:"
  print $ beatsList cards (Card Hearts King) trump
