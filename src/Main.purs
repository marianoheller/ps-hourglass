module Main where

import Prelude
import Config as Conf
import Types
import Math
import Data.List
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


drawStuff :: Effect Unit
drawStuff canvas =  do
  configCanvas canvas
  ctx <- C.getContext2D canvas
  C.setFillStyle ctx "red"
  C.setStrokeStyle ctx "black"
  {- pos <- createInitialPos Conf.particlesQty -}
  {- e <- drawParticle ctx Conf.particleRadius pos -}
  e <- drawParticle ctx Conf.particleRadius <$> (createInitialPos Conf.particlesQty)
  pure unit

configCanvas :: C.CanvasElement -> Effect Unit
configCanvas canvas = do
  C.setCanvasHeight canvas Conf.canvasHeight
  C.setCanvasWidth canvas Conf.canvasWidth


createInitialPos :: Int -> List TupleXY
createInitialPos qty = (\n -> { x: n, y: n }) <<< toNumber <$> (range 1 qty)


drawParticle :: C.Context2D -> Number -> TupleXY -> Effect Unit
drawParticle ctx r p = do
  C.fillPath ctx $ C.arc ctx
    { x: p.x
    , y: p.y
    , radius: r
    , start: 0.0
    , end: pi
    }