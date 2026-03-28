module Main where

import Test.Hspec
import Lib

main :: IO ()
main = hspec $ do
  describe "rankValue" $ do
    it "Ace is worth 1" $
      rankValue Ace `shouldBe` 1
    it "Two is worth 2" $
      rankValue Two `shouldBe` 2
    it "Ten is worth 10" $
      rankValue Ten `shouldBe` 10
    it "Jack is worth 10" $
      rankValue Jack `shouldBe` 10
    it "Queen is worth 10" $
      rankValue Queen `shouldBe` 10
    it "King is worth 10" $
      rankValue King `shouldBe` 10

  describe "handValue" $ do
    it "empty hand is 0" $
      handValue [] `shouldBe` 0
    it "single card returns its rank value" $
      handValue [(Heart, King)] `shouldBe` 10
    it "sums multiple cards" $
      handValue [(Heart, King), (Spade, Five)] `shouldBe` 15
    it "counts Ace as 1 by default" $
      handValue [(Heart, Ace), (Spade, King)] `shouldBe` 11

  describe "containsAce" $ do
    it "returns False for empty hand" $
      containsAce [] `shouldBe` False
    it "returns True when Ace is present" $
      containsAce [(Heart, Ace), (Spade, King)] `shouldBe` True
    it "returns False when no Ace" $
      containsAce [(Heart, King), (Spade, Queen)] `shouldBe` False

  describe "specialHandValue" $ do
    it "adds 10 when hand contains Ace" $
      specialHandValue [(Heart, Ace), (Spade, Five)] `shouldBe` 16
    it "does not add 10 when no Ace" $
      specialHandValue [(Heart, King), (Spade, Five)] `shouldBe` 15

  describe "isBust" $ do
    it "returns False for hand under 21" $
      isBust [(Heart, King), (Spade, Five)] `shouldBe` False
    it "returns True for hand over 21" $
      isBust [(Heart, King), (Spade, Queen), (Diamond, Five)] `shouldBe` True

  describe "bestValue" $ do
    it "returns higher value when Ace can count as 11" $
      bestValue [(Heart, Ace), (Spade, Nine)] `shouldBe` 20
    it "falls back to Ace as 1 when Ace as 11 would bust" $
      bestValue [(Heart, Ace), (Spade, King), (Diamond, Queen)] `shouldBe` 21
    it "returns 0 when bust" $
      bestValue [(Heart, King), (Spade, Queen), (Diamond, Five)] `shouldBe` 0

  describe "winsOver" $ do
    it "higher bestValue wins" $
      winsOver [(Heart, Ace), (Spade, King)] [(Heart, Nine), (Spade, King)] `shouldBe` True
    it "lower bestValue loses" $
      winsOver [(Heart, Nine), (Spade, King)] [(Heart, Ace), (Spade, King)] `shouldBe` False

  describe "dealHands" $ do
    it "deals two hands of 2 cards and returns remaining deck" $ do
      let deck = [(Heart, Ace), (Heart, Two), (Heart, Three), (Heart, Four), (Heart, Five)]
          (hand1, hand2, remaining) = dealHands deck
      hand1 `shouldBe` [(Heart, Ace), (Heart, Two)]
      hand2 `shouldBe` [(Heart, Three), (Heart, Four)]
      remaining `shouldBe` [(Heart, Five)]

  describe "parseBet" $ do
    it "accepts valid bet" $
      parseBet 100 "50" `shouldBe` Right 50
    it "rejects bet above credit" $
      parseBet 100 "150" `shouldBe` Left "Invalid bet"
    it "rejects zero bet" $
      parseBet 100 "0" `shouldBe` Left "Invalid bet"
    it "rejects negative bet" $
      parseBet 100 "-5" `shouldBe` Left "Invalid bet"
    it "rejects non-numeric input" $
      parseBet 100 "abc" `shouldBe` Left "Could not parse bet"

  describe "showCards" $ do
    it "shows all cards when not dealer" $
      showCards [(Heart, Ace), (Spade, King)] False `shouldBe` "(Heart,Ace), (Spade,King)"
    it "hides second card when dealer" $
      showCards [(Heart, Ace), (Spade, King)] True `shouldBe` "(Heart,Ace), X"

  describe "shuffle" $ do
    it "preserves all elements" $ do
      shuffled <- shuffle [1..52 :: Int]
      length shuffled `shouldBe` 52

  describe "cards" $ do
    it "has 52 cards" $
      length cards `shouldBe` 52
