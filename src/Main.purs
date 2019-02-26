module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CANVAS, rect, fillPath, setFillStyle, getContext2D,
                        getCanvasElementById)
import Partial.Unsafe (unsafePartial)

main :: Eff (canvas :: CANVAS) Unit
main = void $ unsafePartial do
  Just canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas
