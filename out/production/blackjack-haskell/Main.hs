module Main where
import Lib
import Shuffle

winningValue = 21

data Suit = Heart | Diamond | Spade | Club deriving(Eq, Show, Enum)
suits = [Heart, Diamond, Spade, Club]

data Rank = King | Queen | Jack | Ten | Nine | Eight | Seven | Six | Five | Four | Three | Two | Ace deriving(Eq, Show, Read)
ranks = [King, Queen, Jack, Ten, Nine, Eight, Seven, Six, Five, Four, Three, Two, Ace]

rankValue :: Rank -> Int
rankValue rank | rank == King || rank == Queen || rank == Jack || rank == Ten = 10
rankValue Nine = 9
rankValue Eight = 8
rankValue Seven = 7
rankValue Six = 6
rankValue Five = 5
rankValue Four = 4
rankValue Three = 3
rankValue Two = 2
rankValue Ace = 1

data Card = Card Suit Rank deriving(Show)

cards :: [Card]
cards = [Card suit rank | suit <- suits, rank <- ranks]

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

main :: IO ()
main = gameLoop (GameState 100) >>= (print . show)

gameLoop :: GameState -> IO GameState
gameLoop (GameState current) = do
  deck <- fmap Deck (shuffle cards)
  let (playerHand, dealerHand, newDeck) = dealHands deck
  playerWon <- roundLoop playerHand dealerHand newDeck False
  let newCredit =
        if playerWon
           then current + 10
           else current - 10
  if newCredit > 0 && newCredit < 1000
    then gameLoop (GameState newCredit)
    else return (GameState newCredit)

roundLoop :: Hand -> Hand -> Deck -> Bool -> IO Bool
roundLoop playerHand dealerHand deck stand = do
  print (show playerHand)
  print (show dealerHand)
  let playerWon = winsOver playerHand dealerHand
  print (show playerWon)
  return playerWon
