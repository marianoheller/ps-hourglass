module Main where

import Prelude
import Config as Conf
import Types
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Graphics.Canvas as C
import Partial.Unsafe (unsafePartial)


main :: forall e. Effect Unit
main = void $ unsafePartial do
  mcanvas <- C.getCanvasElementById "targetCanvas"
  case mcanvas of
    Just canvas -> do
      configCanvas
      ctx <- C.getContext2D canvas
      C.setFillStyle ctx "red"
      C.setStrokeStyle ctx "black"
      poses <- createPos Conf.particlesQty
      fmap (drawParticles ctx Conf.particleRadius) poses
      pure unit
    Nothing -> pure unit

configCanvas :: C.CanvasElement -> Unit
configCanvas canvas = do
  C.setCanvasHeight canvas Conf.canvasHeight
  C.setCanvasWidth canvas Conf.canvasWidth
  unit

createPos :: Number -> List TupleXY
createPos qty = do
  n <- 1.0 .. qty
  pure 

drawParticles :: C.Context2D -> Number -> TupleXY -> Effect Unit
drawParticles ctx r p = do
  C.fillPath ctx $ C.arc ctx
    { x: p.x
    , y: p.y
    , radius: r
    , start: 0.0
    , end: 3.141516
    }