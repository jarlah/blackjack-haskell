module Lib
    ( shuffle
    , Suit(..), Rank(..), Card, Deck, Hand
    , winningValue, winFactor
    , rankValue, cards, handValue, containsAce
    , specialHandValue, isBust, bestValue, winsOver
    , showCards, dealHands, dealerTryToWin
    , RoundData, hitOrStand, parseBet
    ) where

import System.Random
import Text.Read (readEither)

-- | Randomly shuffle a list by repeatedly picking a random element
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  i <- randomRIO (0, length xs - 1)
  let (picked, rest) = removeAt i xs
  remaining <- shuffle rest
  return (picked : remaining)

-- | Remove the element at index n, returning it and the remaining list
removeAt :: Int -> [a] -> (a, [a])
removeAt _ [] = error "removeAt: index out of bounds"
removeAt 0 (x:xs) = (x, xs)
removeAt n (x:xs) =
  let (picked, rest) = removeAt (n - 1) xs
  in (picked, x : rest)

-- Types

data Suit = Heart | Diamond | Spade | Club deriving (Eq, Show, Enum)
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving (Enum, Eq, Show, Bounded)
type Card = (Suit, Rank)
type Deck = [Card]
type Hand = [Card]

type RoundData = (Hand, Hand, Deck, Bool)

-- Constants

winningValue :: Int
winningValue = 21

winFactor :: Double
winFactor = 1.5

-- Pure game logic

rankValue :: Rank -> Int
rankValue King = 10
rankValue Queen = 10
rankValue Jack = 10
rankValue rank = fromEnum rank + 1

cards :: Deck
cards = [(suit, rank) | suit <- [Heart ..], rank <- [Ace ..]]

handValue :: Hand -> Int
handValue [] = 0
handValue ((_, rank):xs) = rankValue rank + handValue xs

containsAce :: Hand -> Bool
containsAce [] = False
containsAce ((_, rank):xs) = rank == Ace || containsAce xs

specialHandValue :: Hand -> Int
specialHandValue hand
  | containsAce hand = handValue hand + 10
  | otherwise = handValue hand

isBust :: Hand -> Bool
isBust hand = handValue hand > winningValue

bestValue :: Hand -> Int
bestValue hand
  | value <= winningValue = value
  | otherwise = 0
  where
    value = max (handValue hand) (specialHandValue hand)

winsOver :: Hand -> Hand -> Bool
winsOver this that = bestValue this > bestValue that

showCards :: Hand -> Bool -> String
showCards [] _ = ""
showCards (card:xs) dealer
  | dealer = show card ++ ", X"
  | null xs = show card
  | otherwise = show card ++ ", " ++ showCards xs False

dealHands :: Deck -> (Hand, Hand, Deck)
dealHands (c1:c2:c3:c4:rest) = ([c1, c2], [c3, c4], rest)
dealHands _ = error "dealHands: deck has fewer than 4 cards"

dealerTryToWin :: Hand -> Deck -> (Hand, Deck)
dealerTryToWin hand (card:rest)
  | handValue hand < 17 = dealerTryToWin (card : hand) rest
dealerTryToWin hand deck = (hand, deck)

hitOrStand :: Hand -> Hand -> Deck -> Bool -> RoundData
hitOrStand playerHand dealerHand (card:rest) True =
  (card : playerHand, dealerHand, rest, False)
hitOrStand playerHand dealerHand deck False =
  let (newDealerHand, newDeck) = dealerTryToWin dealerHand deck
  in (playerHand, newDealerHand, newDeck, True)
hitOrStand _ _ [] True =
  error "hitOrStand: deck is empty"

parseBet :: Int -> String -> Either String Int
parseBet current str = case readEither str of
  Right n | n > 0 && n <= current -> Right n
  Right _ -> Left "Invalid bet"
  Left _ -> Left "Could not parse bet"
