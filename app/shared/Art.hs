{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module defining a function 'Board' -> ASCII. Not in 'Board' itself,
-- because it depends on 'Total' and 'Total' needs to depend on 'Board'.
module Art (toASCII) where

import qualified Board
import Card
import Data.Function ((&))
import Data.List
import Data.Maybe
import Spots
import qualified Total

-- | Converts a 'Board' to an ASCII String, omitting some data for readibility
toASCII :: Board.T 'Core -> String
toASCII board =
  (intersperse "\n" lines & concat) ++ "\n"
  where
    lines =
      stackLines board PlayerTop ++ []
        ++ [handLine board PlayerTop]
        ++ [scoreLine board PlayerTop]
        ++ [decoLine board PlayerTop]
        ++ cardsLines board PlayerTop Spots.backSpots
        ++ ["\n"] -- vertical space between AI lines
        ++ cardsLines board PlayerTop Spots.frontSpots
        ++ ["\n", "\n"] -- vertical space between players
        ++ cardsLines board PlayerBot Spots.frontSpots
        ++ ["\n"] -- vertical space between player lines
        ++ cardsLines board PlayerBot Spots.backSpots
        ++ []
        ++ [decoLine board PlayerBot]
        ++ [scoreLine board PlayerBot]
        ++ [handLine board PlayerBot]
        ++ stackLines board PlayerBot

stackLines :: Board.T 'Core -> Spots.Player -> [String]
stackLines board pSpot =
  map (\s -> replicate 4 ' ' ++ s) $ reverse $ go 0 []
  where
    discarded = Board.getpk @'Board.Discarded pSpot board
    stack = Board.toStack board pSpot
    hspace = replicate 8 ' '
    stackWidth = 16
    justify s | length s < stackWidth = s ++ replicate (stackWidth - length s) ' '
    justify s | length s > stackWidth = take stackWidth s
    justify s = s
    go i acc =
      case (stackLine Board.Discarded' discarded i, stackLine Board.Stacked stack i) of
        (Nothing, Nothing) -> acc
        (d, st) ->
          let line =
                justify (fromMaybe blanks d)
                  ++ hspace
                  ++ justify (fromMaybe blanks st)
           in go (i + 1) (line : acc)
          where
            blanks = replicate stackWidth ' '

handLine :: Board.T 'Core -> Spots.Player -> String
handLine board pSpot =
  "Hand: " ++ intercalate ", " (map showID $ Board.getpk @'Board.Hand pSpot board)

decoLine :: Board.T 'Core -> Spots.Player -> String
decoLine board pSpot
  | null d = "no deco"
  | otherwise = "deco: " ++ show d
  where
    d = Board.toPart board pSpot & Board.deco

scoreLine :: Board.T 'Core -> Spots.Player -> String
scoreLine board pSpot =
  replicate cardWidth ' ' ++ " Score: " ++ show (Board.toScore pSpot board)

stackLine :: Board.StackKind -> [Card.ID] -> LineNumber -> Maybe String
stackLine Board.Discarded' _ 0 = Just "Discarded"
stackLine Board.Stacked _ 0 = Just "Stack"
stackLine _ cards i | i > length cards = Nothing
stackLine _ cards i = Just $ showID $ cards !! (i - 1)

showID :: Card.ID -> String
showID (IDC CreatureID {creatureKind, team} _) =
  showTeamShort team ++ " " ++ show creatureKind
showID (IDI i) = show i
showID (IDN n) = show n

showTeamShort :: Team -> String
showTeamShort = \case
  Evil -> "E"
  Human -> "H"
  Sylvan -> "S"
  Undead -> "UD"
  ZKnights -> "Z"

cardsLines :: Board.T 'Core -> Spots.Player -> [Spots.Card] -> [String]
cardsLines board pSpot cSpots =
  map f [0 .. cardHeight - 1]
  where
    f lineNb =
      let pieces = map (\cSpot -> cardLine board pSpot cSpot lineNb) cSpots
       in intersperse " " pieces & concat -- horizontal space between cards

-- The height of a card, in number of lines
cardHeight :: Int
cardHeight = 1 + 1 + 5

-- The width of a card, in number of characters
cardWidth :: Int
cardWidth = 16

type LineNumber = Int

-- | The line number must be in [0, cardHeight)
cardLine :: Board.T 'Core -> Spots.Player -> Spots.Card -> LineNumber -> String
cardLine board pSpot cSpot lineNb =
  case length base of
    i | i < cardWidth -> base ++ replicate (cardWidth - i) '.'
    i | i > cardWidth -> take cardWidth base
    _ -> base
  where
    maybeCreature = Board.toInPlaceCreature board pSpot cSpot
    emptyLine :: String = replicate cardWidth '.'
    base = case maybeCreature of
      Nothing -> if lineNb == 0 then show cSpot else emptyLine
      Just creature -> fromMaybe emptyLine $ creatureToAscii (Just place) creature lineNb
    place = Total.mkPlace board pSpot cSpot

-- | The n-th line of a creature card, or None
creatureToAscii ::
  p ~ 'Core =>
  Maybe Total.Place ->
  Creature p ->
  LineNumber ->
  Maybe String
creatureToAscii part (c@Creature {creatureId = CreatureID {..}, hp, skills}) =
  \case
    0 -> Just $ show team ++ " " ++ show creatureKind
    1 -> Just $ show hp ++ "<3 " ++ show (Total.attack part c) ++ "X"
    i | (i - 2) `elem` idxes skills -> Just $ show $ skills !! (i - 2)
    _ -> Nothing
  where
    idxes l = [0 .. length l - 1]
