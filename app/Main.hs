{-# LANGUAGE LambdaCase #-}
module Main where
import Lib
import Data.Char
import Text.Read (readMaybe, readEither)
import Data.List (intercalate)
import System.Random (StdGen)

data Suit = Heart | Diamond | Spade | Club deriving(Eq, Show, Enum)
data Rank = Ace | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King deriving(Enum, Eq, Show, Bounded)
type Card = (Suit, Rank)
type Deck = [Card]
type Hand = [Card]

winningValue = 21
winFactor = 1.5

rankValue rank | rank == King || rank == Queen || rank == Jack = 10
rankValue rank = fromEnum rank + 1

cards = [(suit, rank) | suit <- [Heart ..], rank <- [Ace ..]]

handValue [(_, rank)] = rankValue rank
handValue ((_,rank):xs) = rankValue rank + handValue xs

containsAce [(_, rank)] = Ace == rank
containsAce ((_,rank):xs) = Ace == rank || containsAce xs

specialHandValue hand = if containsAce hand then handValue hand + 10 else handValue hand

isBust hand = handValue hand > winningValue

bestValue hand = if value <= winningValue then value else 0 where value = max (handValue hand) (specialHandValue hand)

winsOver this that = bestValue this > bestValue that

showCards :: Hand -> Bool -> String
showCards (card:xs) dealer
  | dealer = show card ++ ", X"
  | not (null xs) = show card ++ ", " ++ showCards xs dealer
  | otherwise = show card

dealHands :: Deck -> (Hand, Hand, Deck)
dealHands deck = (hand1, hand2, newDeck)
  where
    (firstCard, deck1) = (head deck, tail deck)
    (secondCard, deck2) = (head deck1, tail deck1)
    (thirdCard, deck3) = (head deck2, tail deck2)
    (fourthCard, newDeck) = (head deck3, tail deck3)
    hand1 = [firstCard, secondCard]
    hand2 = [thirdCard, fourthCard]

dealerTryToWin :: Hand -> Deck -> (Hand, Deck)
dealerTryToWin hand deck | handValue hand < 17 =
  dealerTryToWin (card : hand) newDeck
  where (card, newDeck) = (head deck, tail deck)
dealerTryToWin hand deck = (hand, deck)

type RoundData = (Hand, Hand, Deck, Bool)

hitOrStand :: Hand -> Hand -> Deck -> Bool -> RoundData
hitOrStand playerHand dealerHand deck hit =
  if hit
    then -- lets hit
      let (card, newDeck) = (head deck, tail deck)
      in (card : playerHand, dealerHand, newDeck, False)
    else -- let the dealer try to win
      let (newDealerHand, newDeck) = dealerTryToWin dealerHand deck
      in (playerHand, newDealerHand, newDeck, True)

parseBet :: Int -> String -> Either String Int
parseBet current str = case readEither str of
                            Right n | n > 0 && n <= current -> Right n
                            Right n | n < 0 || n > current -> Left "Invalid bet"
                            Left e -> Left "Could not parse bet"

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

doAskUser :: String -> [String] -> IO String
doAskUser question possibleAnswers =
  putStrLn (question ++ " (" ++ intercalate "," possibleAnswers ++ ")") >>
  map toLower <$> getLine >>= \case
    line
      | elem line possibleAnswers -> return line
    _ -> doAskUser question possibleAnswers

doAskToContinue = (== "y") <$> doAskUser "Do you want to continue?" ["y", "n"]
doAskToHit = (== "h") <$> doAskUser "Hit or Stand?" ["h", "s"]

doHitOrStand :: Hand -> Hand -> Deck -> IO RoundData
doHitOrStand playerHand dealerHand deck =
  doShowHands playerHand dealerHand True >>
  hitOrStand playerHand dealerHand deck <$> doAskToHit

doRoundLoop :: RoundData -> IO Bool
doRoundLoop (playerHand, dealerHand, deck, stand)
  | isBust playerHand = doShowSummary playerHand dealerHand False
  | stand = doShowSummary playerHand dealerHand (isBust dealerHand || winsOver playerHand dealerHand)
  | otherwise = doHitOrStand playerHand dealerHand deck >>= doRoundLoop

doGameLoop :: Int -> Int -> IO Int
doGameLoop bet currentCredit =
  dealHands <$> (shuffle cards >>= shuffle) >>= \(playerHand, dealerHand, newDeck) ->
    doRoundLoop (playerHand, dealerHand, newDeck, False) >>= \playerWon ->
      let newCredit =
            if playerWon
              then currentCredit + (round (fromIntegral bet * winFactor) :: Int)
              else currentCredit - bet
       in if newCredit > 0
            then putStrLn ("Old credit: " ++ show currentCredit ++ ". New credit: " ++ show newCredit) >>
                 doAskToContinue >>= \continue ->
                   if not continue
                     then return newCredit
                     else doPlaceBet newCredit
            else putStrLn "Game over" >> return newCredit

doPlaceBet :: Int -> IO Int
doPlaceBet current =
  putStrLn ("Place your bet (credit " ++ show current ++ "):") >>
  parseBet current <$> getLine >>= \ case
    Left error ->
      putStrLn ("Error: " ++ error) >>
      doPlaceBet current
    Right bet ->
      doGameLoop bet current

main :: IO ()
main =
  putStrLn "Welcome to BlackJack!" >>
  getLine >>
  doPlaceBet 100 >>= print