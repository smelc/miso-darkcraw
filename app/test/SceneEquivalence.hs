module SceneEquivalence ((~=), forgetKeys) where

import Cinema
  ( ActorState,
    Frame (Frame),
    Scene,
    TimedFrame (TimedFrame),
    render,
  )
import qualified Data.Map as M
import qualified Data.Set as S
import Test.Hspec (Expectation, shouldBe)

data TimedRenderedFrame = TimedRenderedFrame Int (S.Set ActorState)
  deriving (Eq, Show)

forgetKeys :: TimedFrame ActorState -> TimedRenderedFrame
forgetKeys (TimedFrame duration (Frame mapping)) = TimedRenderedFrame duration (S.fromList (M.elems mapping))

(~=) :: Scene () -> Scene () -> Expectation
scene1 ~= scene2 = framesOf scene1 `shouldBe` framesOf scene2
  where
    framesOf scene = map forgetKeys (render scene)
