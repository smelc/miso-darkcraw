{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- This module provides functions to pretty print boards.
--
-- This module has more dependencies than required, because I'm writing
-- functions in it that I use in `cabal repl`. That's okay, this module
-- is for debugging only anyway.
-- |
module Ascii where

import AI
import Board
import Card
import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Text as Text
import Game
import SharedModel
import Turn

-- Functions I'm currently using within `cabal repl`

-- Sadly 'Evaluate...' doesn't work on this in hls:
-- >>> exampleBoardToAscii
exampleBoardToAscii :: String
exampleBoardToAscii =
  startingBoard sharedCards & boardToASCII
  where
    SharedModel {sharedCards} = unsafeGet

exampleBoardPlayOneToAscii =
  case board' of
    Left errMsg -> Text.unpack errMsg
    Right (board', _) -> boardToASCII board'
  where
    SharedModel {sharedCards} = unsafeGet
    board = startingBoard sharedCards
    (turn, turn') = (initialTurn, nextTurn turn)
    events = AI.placeCards board turn'
    board' = Game.playAll board events

-- Now the library code starts

boardToASCII :: Board Core -> String
boardToASCII board =
  (intersperse "\n" lines & concat) ++ "\n"
  where
    lines =
      cardsLines board AITop
        ++ ["\n"] -- vertical space between AI lines
        ++ cardsLines board AIBot
        ++ ["\n", "\n"] -- vertical space between players
        ++ cardsLines board Ascii.PlayerTop
        ++ ["\n"] -- vertical space between player lines
        ++ cardsLines board Ascii.PlayerBot

data BoardLine
  = -- The very top line
    AITop
  | -- The frontline of the AI
    AIBot
  | -- The frontline of the player
    PlayerTop
  | -- The back oline of the player
    PlayerBot

boardLineToSpots AITop = (Board.PlayerTop, topSpots)
boardLineToSpots AIBot = (Board.PlayerTop, botSpots)
boardLineToSpots Ascii.PlayerTop = (Board.PlayerBottom, botSpots)
boardLineToSpots Ascii.PlayerBot = (Board.PlayerBottom, topSpots)

cardsLines :: Board Core -> BoardLine -> [String]
cardsLines board boardLine =
  map (f cSpots) [0 .. cardHeight - 1]
  where
    (pSpot, cSpots) = boardLineToSpots boardLine
    f :: [CardSpot] -> LineNumber -> String
    f cSpots lineNb =
      let pieces = map (\cSpot -> cardLine board pSpot cSpot lineNb) cSpots
       in intersperse " " pieces & concat -- horizontal space between cards

-- The height of a card, in number of lines
cardHeight = 1 + 1 + 5

-- The height of a card, in number of characters
cardWidth = 16

-- | The width of a board, in number of characters. 3 horizontal cards
-- with 2 spaces between cards.
boardWidth = 3 * cardWidth + 2 * 2

type LineNumber = Int

-- | The line number must be in [0, cardHeight)
cardLine :: Board Core -> PlayerSpot -> CardSpot -> LineNumber -> String
cardLine board pSpot cSpot lineNb =
  case length base of
    i | i < cardWidth -> base ++ replicate (cardWidth - i) '.'
    i | i > cardWidth -> take cardWidth base
    _ -> base
  where
    maybeCreature = boardToInPlace board pSpot Map.!? cSpot
    emptyLine :: String = replicate cardWidth '.'
    base = case maybeCreature of
      Nothing -> if lineNb == 0 then show cSpot else emptyLine
      Just creature -> fromMaybe emptyLine $ creatureToAscii creature lineNb

-- | The n-th line of a creature card, or None
creatureToAscii :: Creature Core -> LineNumber -> Maybe String
creatureToAscii Creature {creatureId = CreatureID {..}} 0 =
  Just $ show team ++ " " ++ show creatureKind
creatureToAscii Creature {..} 1 =
  Just $ show hp ++ "<3 " ++ show attack ++ "X"
creatureToAscii _ _ = Nothing
