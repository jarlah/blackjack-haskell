{-# LANGUAGE LambdaCase #-}
module Main where
import Lib
import Data.Char
import Text.Read (readMaybe, readEither)

winningValue :: Int
winningValue = 21

winFactor :: Double
winFactor = 1.5

shuffleTwice :: [Card] -> IO [Card]
shuffleTwice cards = shuffle cards >>= shuffle

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
  | dealer = show rank ++ ", X"
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

dealerTryToWin :: Hand -> Deck -> (Hand, Deck)
dealerTryToWin hand deck | handValue hand < 17 =
  dealerTryToWin (addCard hand card) newDeck
  where (card, newDeck) = deal deck
dealerTryToWin hand deck =
  (hand, deck)

type RoundData = (Hand, Hand, Deck, Bool)

hitOrStand :: Hand -> Hand -> Deck -> Bool -> RoundData
hitOrStand playerHand dealerHand deck hit =
  if hit
    then do -- lets hit
      let (card, newDeck) = deal deck
      let newPlayerHand = addCard playerHand card
      (newPlayerHand, dealerHand, newDeck, False)
    else do -- let the dealer try to win
      let (newDealerHand, newDeck) = dealerTryToWin dealerHand deck
      (playerHand, newDealerHand, newDeck, True)

parseBet :: Int -> String -> Either String Int
parseBet current str = case readEither str of
                            Right n | n > 0 && n <= current -> Right n
                            Right n | n < 0 || n > current -> Left "Invalid bet"
                            Left e -> Left "Could not parse bet"

updateCredit :: Int -> Int -> Bool -> Int
updateCredit current bet playerWon
  | playerWon = current + round (fromIntegral bet * winFactor) :: Int
  | otherwise = current - bet

shouldHit :: String -> Bool
shouldHit line = map toLower line == "h"

shouldContinue :: String -> Bool
shouldContinue line = map toLower line == "y"

-- IO code starts here ---

doShowHands :: Hand -> Hand -> Bool -> IO ()
doShowHands playerHand dealerHand dealer =
  putStrLn (showCards dealerHand dealer) >>
  putStrLn (showCards playerHand False)

doShowSummary :: Hand -> Hand -> Bool -> IO Bool
doShowSummary playerHand dealerHand playerWon =
  doShowHands playerHand dealerHand False >>
  putStrLn ("*** You " ++ (if playerWon then "won" else "lose!") ++ " ***") >>
  return playerWon

doHitOrStand :: Hand -> Hand -> Deck -> IO RoundData
doHitOrStand playerHand dealerHand deck =
  hitOrStand playerHand dealerHand deck <$>
    (doShowHands playerHand dealerHand True >>
     putStrLn "Hit or Stand? (h, s)" >>
     shouldHit <$> getLine)

doRoundLoop :: RoundData -> IO Bool
doRoundLoop (playerHand, dealerHand, deck, stand)
  | isBust playerHand = doShowSummary playerHand dealerHand False
  | stand = doShowSummary playerHand dealerHand (isBust dealerHand || winsOver playerHand dealerHand)
  | otherwise = doHitOrStand playerHand dealerHand deck >>= doRoundLoop

doGameLoop :: Int -> Int -> IO Int
doGameLoop bet current = do
  deck <- Deck <$> shuffleTwice cards
  let (playerHand, dealerHand, newDeck) = dealHands deck
  playerWon <- doRoundLoop (playerHand, dealerHand, newDeck, False)
  let newCredit = updateCredit current bet playerWon
  if newCredit > 0
    then putStrLn ("Old credit: " ++ show current ++ ". New credit: " ++ show newCredit) >>
         putStrLn "Do you want to continue?" >>
         shouldContinue <$> getLine >>=
         (\continue ->
            if not continue
              then return newCredit
              else doPlaceBet newCredit)
    else putStrLn "Game over" >> return newCredit

doPlaceBet :: Int -> IO Int
doPlaceBet current =
  putStrLn ("Place your bet (credit " ++ show current ++ "):") >>
  parseBet current <$> getLine >>=
    (\case
       Left error ->
        putStrLn ("Error: " ++ error) >>
        doPlaceBet current
       Right bet ->
        doGameLoop bet current)

main :: IO ()
main =
  putStrLn "Welcome to BlackJack!" >>
  getLine >>
  doPlaceBet 100 >>= print