import Data.List (find, sort, group, sortOn)
import Data.Maybe (fromMaybe)
import Utils ((<!>), splitByChar)
import Data.Bifunctor (first)

main =
  do
    test

    realData <- readFile "../data/day7.data.txt"

    let games = parse $ lines realData
    print $ part1 games
    print $ part2 games

-- test

test =
  do
    FiveOfAKind  <!> handType (hand "AAAAA")
    FourOfAKind  <!> handType (hand "AA8AA")
    FullHouse    <!> handType (hand "23332")
    ThreeOfAKind <!> handType (hand "TTT98")
    TwoPairs     <!> handType (hand "23432")
    OnePair      <!> handType (hand "A23A4")
    HighCard     <!> handType (hand "23456")

    True <!> (hand "33332" > hand "2AAAA")
    True <!> (hand "77888" > hand "77788")

    testData <- readFile "../data/day7.test.txt"
    let games = parse $ lines testData

    6440 <!> part1 games

    True <!> (Card '2' > Card '?')

    OnePair     <!> handType (hand2 $ hand "32T3K")
    TwoPairs    <!> handType (hand2 $ hand "KK677")
    FourOfAKind <!> handType (hand2 $ hand "T55J5")
    FourOfAKind <!> handType (hand2 $ hand "KTJJT")
    FourOfAKind <!> handType (hand2 $ hand "QQQJA")

    5905 <!> part2 games

    putStrLn "Tests completed"

-- HandType

data HandType = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPairs | OnePair | HighCard
  deriving (Eq, Show)

typeVal :: HandType -> Int
typeVal HighCard     = 0
typeVal OnePair      = 1
typeVal TwoPairs     = 2
typeVal ThreeOfAKind = 3
typeVal FullHouse    = 4
typeVal FourOfAKind  = 5
typeVal FiveOfAKind  = 6

instance Ord HandType where
  a `compare` b = (typeVal a) `compare` (typeVal b)

currentHandType :: [Card] -> HandType
currentHandType cs =
  let
    repCounts = map length $ group $ sort cs
    sortedRepCounts = reverse $ sort repCounts
  in
    case sortedRepCounts of
      [5] -> FiveOfAKind
      [4, 1] -> FourOfAKind
      [3, 2] -> FullHouse
      [3, 1, 1] -> ThreeOfAKind
      [2, 2, 1] -> TwoPairs
      [2, 1, 1, 1] -> OnePair
      [1, 1, 1, 1, 1] -> HighCard

-- Card

newtype Card = Card Char
  deriving (Show, Eq)

cardVal :: Card -> Int
cardVal (Card 'T') = 10
cardVal (Card 'J') = 11
cardVal (Card 'Q') = 12
cardVal (Card 'K') = 13
cardVal (Card 'A') = 14
cardVal (Card '?') = 1
cardVal (Card d)   = read [d]

instance Ord Card where
  a `compare` b = (cardVal a) `compare` (cardVal b)

-- Hand

data Hand = Hand {
  cardList :: [Card],
  handType :: HandType
} deriving Show

instance Ord Hand where
  a `compare` b =
    let
      compareByType = (handType a) `compare` (handType b)
      compareByCard =
        fromMaybe EQ $
          find ((/=) EQ) $
            map (uncurry compare) $
              (cardList a) `zip` (cardList b)
    in
      case compareByType of
        EQ -> compareByCard
        _  -> compareByType

instance Eq Hand where
  a == b = (a `compare` b) == EQ

hand :: [Char] -> Hand
hand cs =
  let
    cards = map Card cs
  in
    Hand cards (currentHandType cards)

-- parse

type Bid = Int

parse :: [String] -> [(Hand, Bid)]
parse =
  let
    parseLine l =
      case splitByChar ' ' l of
        cards:bid:[] -> (hand cards, read bid)
  in
    map parseLine

-- part1

zipWithIndexStarting :: Int -> [a] -> [(Int, a)]
zipWithIndexStarting start = zip [start..]

part1 = sum . map (uncurry (*)) . zipWithIndexStarting 1 . map snd . sortOn fst

-- hand2

replace :: (Eq a) => a -> a -> [a] -> [a]
replace toReplace replaceBy =
  let
    f x =
      if x == toReplace
        then replaceBy
        else x
  in
    map f

hand2 :: Hand -> Hand
hand2 (Hand oldCards _) =
  let
    replacedCards = replace (Card 'J') (Card '?') oldCards
  in
    Hand replacedCards (bestHandType replacedCards)

bestHandType :: [Card] -> HandType
bestHandType cards =
  let
    nonJokerCards = map Card "TQKA98765432"
    handTypeAssuming r = currentHandType $ replace (Card '?') r cards
  in
    maximum $ map handTypeAssuming nonJokerCards

-- part2

part2 = part1 . map (first hand2)
