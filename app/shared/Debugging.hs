{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Debugging where

import Data.Bifunctor (first)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as S
import Miso
import Miso.String (MisoString, fromMisoString, ms)
import Text.Pretty.Simple (pShowNoColor)

data DebuggingFrame e m = DebuggingFrame e m
  deriving (Eq)

data DebuggingModel e m
  = Running (Seq (DebuggingFrame e m))
  | Debugging Int (Seq (DebuggingFrame e m)) MisoString
  deriving (Eq)

data DebuggingEvent e
  = LiftEvent e
  | EnterDebugging
  | ResumeFromHere
  | JumpToFrame Int
  | ShowFullModel
  deriving (Eq)

debugApp :: (Show e, Show m) => e -> App m e -> App (DebuggingModel e m) (DebuggingEvent e)
debugApp noop App {..} =
  App
    { model = Running (S.singleton (DebuggingFrame noop model)),
      update = debugUpdate update,
      view = debugView view,
      subs = map (mapSub LiftEvent) subs,
      events = events,
      initialAction = LiftEvent initialAction,
      mountPoint = mountPoint,
      logLevel = logLevel
    }

debugView :: (Show e, Show m) => (m -> View a) -> DebuggingModel e m -> View (DebuggingEvent a)
debugView _ (Running Empty) = error "debugging model should have at least one frame"
debugView view (Running (_ :|> DebuggingFrame _ m)) =
  div_
    [style_ ("display" =: "flex" <> "align-items" =: "start")]
    [ fmap LiftEvent (view m),
      button_
        [onClick EnterDebugging, style_ ("margin-left" =: "20px")]
        [text "enter debugging"]
    ]
debugView view (Debugging i frames renderedModel)
  | Just (DebuggingFrame e m) <- S.lookup i frames =
    div_
      [style_ ("display" =: "flex")]
      [ fmap LiftEvent (view m),
        div_
          [style_ ("margin-left" =: "20px")]
          [ input_
              [ style_ ("width" =: "500px"),
                id_ "debug-slider",
                type_ "range",
                min_ "0",
                max_ (ms $ length frames - 1),
                step_ "1'",
                value_ (ms i),
                onInput (JumpToFrame . read . fromMisoString),
                autofocus_ True
              ],
            label_
              [for_ "debug-slider"]
              [text (ms $ show i <> "/" <> show (length frames))],
            button_
              [onClick ResumeFromHere, style_ ("display" =: "block")]
              [text "resume from here"],
            button_
              [onClick ShowFullModel, style_ ("display" =: "block")]
              [text "show full model"],
            pre_ [] [text (ms $ pShowNoColor e)],
            pre_ [] [text renderedModel]
          ]
      ]
  | otherwise = error "out of bounds"

debugUpdate ::
  Show m =>
  (e -> m -> Effect e m) ->
  DebuggingEvent e ->
  DebuggingModel e m ->
  Effect (DebuggingEvent e) (DebuggingModel e m)
debugUpdate _ _ (Running Empty) = error "cannot update empty model"
debugUpdate update (LiftEvent e) (Running frames@(_ :|> DebuggingFrame _ m)) = do
  m' <- first LiftEvent (update e m)
  return (Running (frames :|> DebuggingFrame e m'))
debugUpdate _ EnterDebugging (Running frames@(_ :|> DebuggingFrame _ m)) =
  noEff $ Debugging (length frames - 1) frames (prefix m)
debugUpdate _ ResumeFromHere (Debugging i frames _) =
  noEff $ Running (S.take (i + 1) frames)
debugUpdate _ (JumpToFrame i) (Debugging _ frames _)
  | Just (DebuggingFrame _ m) <- S.lookup i frames =
    noEff $ Debugging i frames (prefix m)
debugUpdate _ ShowFullModel (Debugging i frames@(_ :|> DebuggingFrame _ m) _) =
  noEff $ Debugging i frames (ms $ pShowNoColor m)
debugUpdate _ _ m = noEff m

prefix :: Show a => a -> MisoString
prefix m = ms $ take 30 (show m) <> "..."
