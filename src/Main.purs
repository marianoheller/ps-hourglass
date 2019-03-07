module Main where

import Prelude
import Config as Conf
import Types
import Data.List
import Data.Int
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Graphics.Canvas as C


main :: Effect Unit
main = do
  mcanvas <- C.getCanvasElementById "targetCanvas"
  case mcanvas of
    Just canvas -> do
      configCanvas
      ctx <- C.getContext2D canvas
      C.setFillStyle ctx "red"
      C.setStrokeStyle ctx "black"
      poses <- createPos Conf.particlesQty
      (drawParticles ctx Conf.particleRadius) <$> poses
      pure unit
    Nothing -> pure unit


configCanvas :: C.CanvasElement -> Effect Unit
configCanvas canvas = do
  C.setCanvasHeight canvas Conf.canvasHeight
  C.setCanvasWidth canvas Conf.canvasWidth
  pure unit


createPos :: Int -> List TupleXY
createPos qty = (\n -> { x: n, y: n }) <$> toNumber <$> (range 1 qty)


drawParticles :: C.Context2D -> Number -> TupleXY -> Effect Unit
drawParticles ctx r p = do
  C.fillPath ctx $ C.arc ctx
    { x: p.x
    , y: p.y
    , radius: r
    , start: 0.0
    , end: 3.141516
    }