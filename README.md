# blackjack-haskell

A terminal blackjack game built with [Brick](https://github.com/jtdaugherty/brick), featuring ASCII card art with colored suits.

```
         === BLACKJACK ===

  Dealer's hand:
  ┌─────┐ ┌─────┐
  │K    │ │░░░░░│
  │  ♠  │ │░░░░░│
  │    K│ │░░░░░│
  └─────┘ └─────┘

  Your hand:                     Value: 20
  ┌─────┐ ┌─────┐
  │A    │ │9    │
  │  ♥  │ │  ♦  │
  │    A│ │    9│
  └─────┘ └─────┘

  Credit: 100  |  Bet: 25
  Hit (H) or Stand (S)?   [Q] Quit
```

## How to play

1. Place a bet (type a number, press Enter)
2. Hit (H) to draw a card, Stand (S) to let the dealer play
3. Beat the dealer without going over 21
4. Aces count as 1 or 11 (whichever is better)
5. Winning pays 1.5x your bet

## Getting started

### Prerequisites

- [GHCup](https://www.haskell.org/ghcup/) (installs Stack and GHC)

```bash
brew install ghcup
ghcup install stack
```

### Build and run

```bash
stack build
stack exec blackjack-haskell-exe
```

### Run tests

```bash
stack test
```

## Project structure

```
src/
  Lib.hs        -- Pure game logic: types, hand evaluation, dealing, shuffling
  CardArt.hs    -- ASCII card rendering with Brick widgets and color
app/
  Main.hs       -- Brick TUI application: state machine, event handling, rendering
test/
  Spec.hs       -- HSpec tests for all pure game logic
```

## Architecture

The app uses the **Model-View-Update** pattern (same as Elm):

- **Model** (`GameState`) holds the current phase, credit, bet, and hands
- **View** (`drawUI`) renders the state as terminal widgets (pure function)
- **Update** (`handleEvent`) handles key presses and produces new state

Game phases: `Betting` -> `Playing` -> `RoundResult` -> back to `Betting` (or `GameOver`)

All game logic (hand evaluation, dealing, shuffling) is pure and lives in `Lib.hs`, making it easy to test independently from the UI.

## Controls

| Key | Action |
|-----|--------|
| `0-9` + `Enter` | Place bet |
| `H` | Hit (draw a card) |
| `S` | Stand (dealer plays) |
| `Y` | Continue (next round) |
| `N` | Quit after round |
| `Q` | Quit anytime |
