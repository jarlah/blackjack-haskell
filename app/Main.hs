module Main where

import Lib
import CardArt

import Brick
import Brick.Widgets.Border (hBorder)
import Brick.Widgets.Center (hCenter)
import qualified Graphics.Vty as Vty

import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower, isDigit)

-- Game phases

data Phase
    = Betting String            -- bet input string typed so far
    | Playing Hand Hand Deck    -- playerHand, dealerHand, remainingDeck
    | RoundResult Hand Hand Bool     -- playerHand, dealerHand, playerWon
    | GameOver
    deriving (Show)

-- Top-level game state

data GameState = GameState
    { gsCredit  :: Int
    , gsBet     :: Int
    , gsPhase   :: Phase
    , gsMessage :: String
    } deriving (Show)

-- Brick requires a resource name type (we don't use named resources)
type Name = ()

-- Initial state with 100 credit

initialState :: GameState
initialState = GameState
    { gsCredit  = 100
    , gsBet     = 0
    , gsPhase   = Betting ""
    , gsMessage = "Type your bet and press Enter."
    }

-- Display the best hand value, showing actual value even when bust

displayValue :: Hand -> String
displayValue hand
    | isBust hand = show (handValue hand) ++ " (BUST)"
    | otherwise   = show (bestValue hand)

-- Event handling

handleEvent :: BrickEvent Name e -> EventM Name GameState ()
handleEvent (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt
handleEvent (VtyEvent (Vty.EvKey key mods)) = do
    st <- get
    case gsPhase st of
        Betting input   -> handleBetting key mods input
        Playing ph dh d -> handlePlaying key ph dh d
        RoundResult _ _ _    -> handleRoundResult key
        GameOver        -> halt
handleEvent _ = return ()

handleBetting :: Vty.Key -> [Vty.Modifier] -> String -> EventM Name GameState ()
handleBetting (Vty.KChar c) _ input
    | isDigit c = modify $ \st ->
        st { gsPhase = Betting (input ++ [c]), gsMessage = "" }
handleBetting Vty.KBS _ input = modify $ \st ->
    st { gsPhase = Betting (safeInit input), gsMessage = "" }
handleBetting Vty.KEnter _ input = do
    st <- get
    case parseBet (gsCredit st) input of
        Left err  -> put st { gsPhase = Betting "", gsMessage = err }
        Right bet -> do
            deck <- liftIO (shuffle cards)
            let (ph, dh, remaining) = dealHands deck
            put st { gsBet    = bet
                   , gsPhase  = Playing ph dh remaining
                   , gsMessage = "Hit (H) or Stand (S)?"
                   }
handleBetting _ _ _ = return ()

handlePlaying :: Vty.Key -> Hand -> Hand -> Deck -> EventM Name GameState ()
handlePlaying (Vty.KChar c) playerHand dealerHand deck
    | toLower c == 'h' =
        let (ph', dh', deck', _) = hitOrStand playerHand dealerHand deck True
        in if isBust ph'
            then finishRound ph' dh' False
            else modify $ \st ->
                st { gsPhase = Playing ph' dh' deck'
                   , gsMessage = "Hit (H) or Stand (S)?"
                   }
    | toLower c == 's' =
        let (ph', dh', _, _) = hitOrStand playerHand dealerHand deck False
            playerWon = isBust dh' || winsOver ph' dh'
        in finishRound ph' dh' playerWon
handlePlaying _ _ _ _ = return ()

handleRoundResult :: Vty.Key -> EventM Name GameState ()
handleRoundResult (Vty.KChar c)
    | toLower c == 'y' = modify $ \st ->
        st { gsPhase = Betting "", gsMessage = "Type your bet and press Enter." }
    | toLower c == 'n' = halt
handleRoundResult _ = return ()

finishRound :: Hand -> Hand -> Bool -> EventM Name GameState ()
finishRound playerHand dealerHand playerWon = modify $ \st ->
    let newCredit = if playerWon
            then gsCredit st + (round (fromIntegral (gsBet st) * winFactor) :: Int)
            else gsCredit st - gsBet st
        msg = if playerWon then "You won!" else "You lost!"
    in st { gsCredit = newCredit
          , gsPhase  = if newCredit <= 0
                       then GameOver
                       else RoundResult playerHand dealerHand playerWon
          , gsMessage = msg ++ " Continue (Y) or Quit (N)?"
          }

safeInit :: String -> String
safeInit [] = []
safeInit s  = init s

-- Rendering

drawUI :: GameState -> [Widget Name]
drawUI st = [ui]
  where
    ui = vBox
        [ hCenter $ str " "
        , hCenter $ withAttr (attrName "title") $ str "=== BLACKJACK ==="
        , hCenter $ str " "
        , hBorder
        , hCenter $ drawPhase (gsPhase st)
        , hBorder
        , hCenter $ drawStatusBar st
        , hCenter $ str " "
        ]

drawPhase :: Phase -> Widget Name
drawPhase (Betting input) =
    vBox [ str " "
         , str "  Place your bet:"
         , str $ "  > " ++ input ++ "_"
         , str " "
         ]
drawPhase (Playing playerHand dealerHand _) =
    vBox [ str " "
         , str "  Dealer's hand:"
         , indent (renderHand dealerHand True)
         , str " "
         , str $ "  Your hand:                     Value: " ++ displayValue playerHand
         , indent (renderHand playerHand False)
         , str " "
         ]
  where
    indent w = hBox [str "  ", w]
drawPhase (RoundResult playerHand dealerHand _) =
    vBox [ str " "
         , str $ "  Dealer's hand:                 Value: " ++ displayValue dealerHand
         , indent (renderHand dealerHand False)
         , str " "
         , str $ "  Your hand:                     Value: " ++ displayValue playerHand
         , indent (renderHand playerHand False)
         , str " "
         ]
  where
    indent w = hBox [str "  ", w]
drawPhase GameOver =
    vBox [ str " "
         , str "  GAME OVER - You ran out of credit!"
         , str "  Press any key to exit."
         , str " "
         ]

drawStatusBar :: GameState -> Widget Name
drawStatusBar st = vBox
    [ str $ "  Credit: " ++ show (gsCredit st) ++ betStr
    , str $ "  " ++ gsMessage st
    , str "  [Q] Quit"
    ]
  where
    betStr = case gsPhase st of
        Betting _ -> ""
        _         -> "  |  Bet: " ++ show (gsBet st)

-- Attribute map for colors

theAttrMap :: AttrMap
theAttrMap = attrMap Vty.defAttr
    [ (redCardAttr,        fg Vty.red)
    , (whiteCardAttr,      fg Vty.white)
    , (faceDownAttr,       fg Vty.blue)
    , (attrName "title",   Vty.withStyle (fg Vty.yellow) Vty.bold)
    ]

-- Brick app definition

app :: App GameState e Name
app = App
    { appDraw         = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent  = handleEvent
    , appStartEvent   = return ()
    , appAttrMap      = const theAttrMap
    }

-- Entry point

main :: IO ()
main = do
    finalState <- defaultMain app initialState
    putStrLn $ "Thanks for playing! Final credit: " ++ show (gsCredit finalState)
