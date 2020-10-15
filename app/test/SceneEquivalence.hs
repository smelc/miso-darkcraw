module SceneEquivalence ((~=), forgetKeys) where

import Cinema
  ( Actor,
    Frame (Frame),
    Scene,
    TimedFrame (TimedFrame),
    render,
  )
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as T
import Test.Hspec (Expectation, shouldBe)
import Text.Pretty.Simple (pShowNoColor)

data TimedRenderedFrame = TimedRenderedFrame Int (S.Set Actor)
  deriving (Eq, Show)

forgetKeys :: TimedFrame -> TimedRenderedFrame
forgetKeys (TimedFrame duration (Frame mapping)) = TimedRenderedFrame duration (S.fromList (M.elems mapping))

newtype Pretty a = Pretty a
  deriving (Eq, Ord)

instance Show a => Show (Pretty a) where
  show (Pretty x) = T.unpack (pShowNoColor x)

(~=) :: Scene () -> Scene () -> Expectation
scene1 ~= scene2 = Pretty (framesOf scene1) `shouldBe` Pretty (framesOf scene2)
  where
    framesOf scene = map forgetKeys (render scene)
