{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Debugging where

import Data.Bifunctor (first)
import Data.Sequence (Seq (..))
import qualified Data.Sequence as S
import Miso
import Miso.String (MisoString, fromMisoString, ms)
import Text.Pretty.Simple (pShowNoColor)
import ViewInternal (flexColumnStyle)

data DebuggingFrame e m = DebuggingFrame e m
  deriving (Eq)

data NoOpMode e m = ShowNoOps | HideNoOps (Seq (DebuggingFrame e m))
  deriving (Eq)

data DebuggingModel e m
  = Running (Seq (DebuggingFrame e m))
  | Debugging (NoOpMode e m) Int (Seq (DebuggingFrame e m)) MisoString
  deriving (Eq)

data DebuggingEvent e
  = LiftEvent e
  | EnterDebugging
  | ResumeFromHere
  | JumpToFrame Int
  | ShowModel
  | SetHideNoOps Checked
  deriving (Eq)

isNoOpFrame :: Eq e => e -> DebuggingFrame e m -> Bool
isNoOpFrame noop (DebuggingFrame e _) = e == noop

hasNoOpFrames :: (Foldable t, Eq e) => e -> t (DebuggingFrame e m) -> Bool
hasNoOpFrames noop frames = any (isNoOpFrame noop) frames

indexWhithoutNoOps :: Eq e => e -> Seq (DebuggingFrame e m) -> Int -> Int
indexWhithoutNoOps noop frames i =
  max
    0
    (i - length (S.filter (isNoOpFrame noop) (S.take (i + 1) frames)))

indexWithNoOps :: Eq e => e -> Seq (DebuggingFrame e m) -> Int -> Int
indexWithNoOps noop frames i = count i frames
  where
    count 0 _ = 0
    count i (frame :<| frames)
      | isNoOpFrame noop frame = 1 + count i frames
      | otherwise = 1 + count (i - 1) frames
    count _ Empty = error "unexepected empty frames"

debugApp :: (Eq e, Show e, Show m) => e -> App m e -> App (DebuggingModel e m) (DebuggingEvent e)
debugApp noop App {..} =
  App
    { model = Running (S.singleton (DebuggingFrame noop model)),
      update = debugUpdate noop update,
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
    [style_ flexColumnStyle]
    [ fmap LiftEvent (view m),
      button_
        [onClick EnterDebugging, style_ ("margin-left" =: "20px")]
        [text "enter debugging"]
    ]
debugView view (Debugging noOpMode i frames renderedModel)
  | Just (DebuggingFrame e m) <- S.lookup i frames =
      div_
        [style_ ("display" =: "flex")]
        [ fmap LiftEvent (view m),
          div_
            [style_ ("margin-left" =: "20px")]
            [ button_
                [ onClick ResumeFromHere,
                  style_ ("display" =: "block" <> "margin-bottom" =: "20px")
                ]
                [text "resume from selected frame"],
              input_
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
              div_
                [style_ ("display" =: "block")]
                [ input_
                    [ id_ "noop-checkbox",
                      type_ "checkbox",
                      checked_ (noOpsHidden noOpMode),
                      onChecked SetHideNoOps
                    ],
                  label_
                    [for_ "noop-checkbox"]
                    [text "hide no-ops"]
                ],
              button_
                [onClick ShowModel, style_ ("display" =: "block")]
                [text "show model"],
              pre_ [] [text (ms $ pShowNoColor e)],
              pre_ [] [text renderedModel]
            ]
        ]
  | otherwise = error "out of bounds"
  where
    noOpsHidden (HideNoOps _) = True
    noOpsHidden ShowNoOps = False

debugUpdate ::
  (Eq e, Show m) =>
  e ->
  (e -> m -> Effect e m) ->
  DebuggingEvent e ->
  DebuggingModel e m ->
  Effect (DebuggingEvent e) (DebuggingModel e m)
debugUpdate _ update (LiftEvent e) (Running frames@(_ :|> DebuggingFrame _ m)) = do
  m' <- first LiftEvent (update e m)
  return (Running (frames :|> DebuggingFrame e m'))
debugUpdate noop _ e m = update e m
  where
    update _ (Running Empty) = error "cannot update empty model"
    update EnterDebugging (Running frames) =
      let trimmedFrames = S.filter (not . isNoOpFrame noop) frames
       in noEff $ Debugging (HideNoOps frames) (length trimmedFrames - 1) trimmedFrames "[...]"
    update ResumeFromHere (Debugging ShowNoOps i frames _) =
      noEff $ Running (S.take (i + 1) frames)
    update ResumeFromHere (Debugging (HideNoOps frames) i _ _) =
      noEff $ Running (S.take (indexWithNoOps noop frames i + 1) frames)
    update (JumpToFrame i) (Debugging noOpMode _ frames _)
      | Just _ <- S.lookup i frames =
          noEff $ Debugging noOpMode i frames "[...]"
    update ShowModel (Debugging noOpMode i frames@(_ :|> DebuggingFrame _ m) _) =
      noEff $ Debugging noOpMode i frames (ms $ pShowNoColor m)
    update (SetHideNoOps (Checked False)) (Debugging (HideNoOps frames) i _ str) =
      noEff $ Debugging ShowNoOps (indexWithNoOps noop frames i) frames str
    update (SetHideNoOps (Checked True)) (Debugging ShowNoOps i frames str)
      | hasNoOpFrames noop frames =
          let trimmedFrames = S.filter (not . isNoOpFrame noop) frames
           in noEff $ Debugging (HideNoOps frames) (indexWhithoutNoOps noop frames i) trimmedFrames str
    update _ m = noEff m
