module Main where
import Lib
import Data.Char (toLower)

winningValue = 21

data Suit = Heart | Diamond | Spade | Club deriving(Eq, Show, Enum)

data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving(Enum, Eq, Show, Bounded)

rankValue :: Rank -> Int
rankValue rank | rank == King || rank == Queen || rank == Jack = 10
rankValue rank = fromEnum rank + 1

data Card = Card Suit Rank deriving(Show)

cards :: [Card]
cards = [Card suit rank | suit <- [Heart ..], rank <- [Ace ..]]

newtype Deck = Deck [Card] deriving(Show)

deal :: Deck -> (Card, Deck)
deal (Deck (topCard:remainingCards)) =
  (topCard, Deck remainingCards)

newtype Hand = Hand [Card] deriving(Show)

handValue :: Hand -> Int
handValue (Hand [Card suit rank]) = rankValue rank
handValue (Hand (Card suit rank:remainingCards)) = rankValue rank + handValue (Hand remainingCards)

containsAce :: Hand -> Bool
containsAce (Hand [Card suit rank]) = Ace == rank
containsAce (Hand (Card suite rank:xs)) = Ace == rank || containsAce (Hand xs)

specialHandValue :: Hand -> Int
specialHandValue hand =
  if containsAce hand
    then value + 10
    else value
  where value = handValue hand

isBlackJack :: Hand -> Bool
isBlackJack hand = handValue hand == winningValue || specialHandValue hand == winningValue

isBust :: Hand -> Bool
isBust hand = handValue hand > winningValue

bestValue :: Hand -> Int
bestValue hand =
  if value <= winningValue
    then value
    else 0
  where value = max (handValue hand) (specialHandValue hand)

winsOver :: Hand -> Hand -> Bool
winsOver this that = bestValue this > bestValue that

showCards :: Hand -> Bool -> String
showCards (Hand (Card suit rank:xs)) dealer
  | dealer = show rank ++ " X"
  | not (null xs) = show rank ++ ", " ++ showCards (Hand xs) dealer
  | otherwise = show rank

addCard :: Hand -> Card -> Hand
addCard (Hand cards) card = Hand (card : cards)

dealHands :: Deck -> (Hand, Hand, Deck)
dealHands deck = (hand1, hand2, newDeck)
  where
    (firstCard, deck1) = deal deck
    (secondCard, deck2) = deal deck1
    (thirdCard, deck3) = deal deck2
    (fourthCard, newDeck) = deal deck3
    hand1 = Hand [firstCard, secondCard]
    hand2 = Hand [thirdCard, fourthCard]

newtype GameState = GameState Int deriving(Show)

newtype RoundData = RoundData (Hand, Hand, Deck, Bool)

main :: IO ()
main = do
  print "Welcome to BlackJack!"
  continue <- getLine
  gameLoop (GameState 100) >>= (print . show)

gameLoop :: GameState -> IO GameState
gameLoop (GameState current) = do
  deck <- fmap Deck (shuffle cards)
  print ("Place your bet (credit " ++ show current ++ "):")
  betAsString <- getLine
  let betAsInt = read betAsString :: Int -- TODO can crash
  let (playerHand, dealerHand, newDeck) = dealHands deck
  playerWon <- roundLoop (RoundData (playerHand, dealerHand, newDeck, False))
  let newCredit = if playerWon then current + betAsInt else current - betAsInt
  print ("Old credit: " ++ show current ++ ". New credit: " ++ show newCredit)
  print "Do you want to continue?"
  continue <- fmap (map toLower) getLine
  if newCredit > 0 && continue == "y"
    then gameLoop (GameState newCredit)
    else return (GameState newCredit)

roundLoop :: RoundData -> IO Bool
roundLoop (RoundData (playerHand, dealerHand, deck, stand))
  | isBust playerHand = do
    print (show dealerHand)
    print (show playerHand)
    print "*** You lose! ***"
    return False
  | stand = do
    let playerWon = isBust dealerHand || winsOver playerHand dealerHand
    print (show dealerHand)
    print (show playerHand)
    print ("*** You " ++ (if playerWon then "won" else "lose!") ++ " ***")
    return playerWon
  | otherwise = hitOrStand playerHand dealerHand deck >>= roundLoop

hitOrStand :: Hand -> Hand -> Deck -> IO RoundData
hitOrStand playerHand dealerHand deck = do
  print (show dealerHand)
  print (show playerHand)
  print "Hit or Stand? (h, s)"
  hitOrStand <- fmap (map toLower) getLine
  if hitOrStand == "h"
    then do
      let (card, newDeck) = deal deck
      let newPlayerHand = addCard playerHand card
      return (RoundData (newPlayerHand, dealerHand, newDeck, False))
    else do
      let (newDealerHand, newDeck) = dealerTryToWin dealerHand deck
      return (RoundData (playerHand, newDealerHand, newDeck, True))

dealerTryToWin :: Hand -> Deck -> (Hand, Deck)
dealerTryToWin hand deck | handValue hand < 17 =
  dealerTryToWin (addCard hand card) newDeck
  where (card, newDeck) = deal deck
dealerTryToWin hand deck =
  (hand, deck)