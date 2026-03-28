module CardArt
    ( renderCard
    , renderFaceDown
    , renderHand
    , redCardAttr
    , whiteCardAttr
    , faceDownAttr
    ) where

import Brick (Widget, str, hBox, vBox, withAttr)
import Brick.AttrMap (AttrName, attrName)
import Lib (Suit(..), Rank(..), Card, Hand)

-- Attribute names for color styling
redCardAttr :: AttrName
redCardAttr = attrName "redCard"

whiteCardAttr :: AttrName
whiteCardAttr = attrName "whiteCard"

faceDownAttr :: AttrName
faceDownAttr = attrName "faceDown"

suitSymbol :: Suit -> String
suitSymbol Heart   = "\x2665"
suitSymbol Diamond = "\x2666"
suitSymbol Spade   = "\x2660"
suitSymbol Club    = "\x2663"

rankLabel :: Rank -> String
rankLabel Ace   = "A"
rankLabel Two   = "2"
rankLabel Three = "3"
rankLabel Four  = "4"
rankLabel Five  = "5"
rankLabel Six   = "6"
rankLabel Seven = "7"
rankLabel Eight = "8"
rankLabel Nine  = "9"
rankLabel Ten   = "10"
rankLabel Jack  = "J"
rankLabel Queen = "Q"
rankLabel King  = "K"

suitAttr :: Suit -> AttrName
suitAttr Heart   = redCardAttr
suitAttr Diamond = redCardAttr
suitAttr Spade   = whiteCardAttr
suitAttr Club    = whiteCardAttr

-- | Render a single card as a 7x5 ASCII art widget with suit color.
renderCard :: Card -> Widget n
renderCard (suit, rank) =
    withAttr (suitAttr suit) $ vBox $ map str
        [ "\x250C\x2500\x2500\x2500\x2500\x2500\x2510"
        , "\x2502" ++ padRight rl ++ "\x2502"
        , "\x2502  " ++ sym ++ "  \x2502"
        , "\x2502" ++ padLeft rl ++ "\x2502"
        , "\x2514\x2500\x2500\x2500\x2500\x2500\x2518"
        ]
  where
    rl  = rankLabel rank
    sym = suitSymbol suit
    -- Top-left: rank left-aligned, Bottom-right: rank right-aligned
    padRight s = s ++ replicate (5 - length s) ' '
    padLeft  s = replicate (5 - length s) ' ' ++ s

-- | Render a face-down card.
renderFaceDown :: Widget n
renderFaceDown =
    withAttr faceDownAttr $ vBox $ map str
        [ "\x250C\x2500\x2500\x2500\x2500\x2500\x2510"
        , "\x2502\x2591\x2591\x2591\x2591\x2591\x2502"
        , "\x2502\x2591\x2591\x2591\x2591\x2591\x2502"
        , "\x2502\x2591\x2591\x2591\x2591\x2591\x2502"
        , "\x2514\x2500\x2500\x2500\x2500\x2500\x2518"
        ]

-- | Render a hand of cards side-by-side. If hideSecond is True,
--   only the first card is shown face-up, the rest are face-down.
renderHand :: Hand -> Bool -> Widget n
renderHand [] _ = str ""
renderHand [c] _ = renderCard c
renderHand (c:rest) hideSecond
    | hideSecond = hBox [renderCard c, str " ", renderFaceDown]
    | otherwise  = hBox $ interleave (map renderCard (c:rest))
  where
    interleave []     = []
    interleave [w]    = [w]
    interleave (w:ws) = w : str " " : interleave ws
