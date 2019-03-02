module Main where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Graphics.Canvas ({- CanvasElement ,  -}rect, fillPath, setFillStyle, setStrokeStyle,
                        getContext2D, getCanvasElementById)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "targetCanvas"
  ctx <- getContext2D canvas
  setFillStyle ctx "red"
  setStrokeStyle ctx "black"
  fillPath ctx $ rect ctx
    { x: 250.0
    , y: 250.0
    , width: 100.0
    , height: 100.0
    }
