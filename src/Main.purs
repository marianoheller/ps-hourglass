module Main where

import Prelude
import Config as Conf
import Types
import Math
import Data.Array
import Data.Traversable (sequence, fold)
import Data.Int
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Graphics.Canvas as C


main :: Effect Unit
main = do
  mcanvas <- C.getCanvasElementById "targetCanvas"
  case mcanvas of
    Just canvas -> drawStuff canvas
    Nothing -> pure unit


drawStuff :: C.CanvasElement -> Effect Unit
drawStuff canvas = do
  configCanvas canvas
  ctx <- C.getContext2D canvas
  C.setFillStyle ctx "red"
  C.setStrokeStyle ctx "black"
  let poses = createInitialPos Conf.particlesQty Conf.canvasWidth Conf.canvasHeight
  map fold $
    sequence $
      drawParticle ctx Conf.particleRadius <$> poses


configCanvas :: C.CanvasElement -> Effect Unit
configCanvas canvas = do
  C.setCanvasHeight canvas Conf.canvasHeight
  C.setCanvasWidth canvas Conf.canvasWidth


createInitialPos :: Int -> Number -> Number -> Array TupleXY
createInitialPos qty width height =
  (\n -> { x: n, y: height / 2.0 }) <<< (\n -> n * step) <<< toNumber <$> (range 1 qty)
  where step = width / qty'
        qty' = toNumber qty


drawParticle :: C.Context2D -> Number -> TupleXY -> Effect Unit
drawParticle ctx r p = do
  C.fillPath ctx $ C.arc ctx
    { x: p.x
    , y: p.y
    , radius: r
    , start: 0.0
    , end: 2.0 * pi
    }
  pure unit