{-# LANGUAGE LambdaCase #-}
module Main where

import Lib
import Data.Char (toLower)
import Data.List (intercalate)

-- IO code

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

doAskToContinue :: IO Bool
doAskToContinue = (== "y") <$> doAskUser "Do you want to continue?" ["y", "n"]

doAskToHit :: IO Bool
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
  dealHands <$> shuffle cards >>= \(playerHand, dealerHand, newDeck) ->
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
  parseBet current <$> getLine >>= \case
    Left err ->
      putStrLn ("Error: " ++ err) >>
      doPlaceBet current
    Right bet ->
      doGameLoop bet current

main :: IO ()
main =
  putStrLn "Welcome to BlackJack!" >>
  doPlaceBet 100 >>= print
