module Main where

import Control.Monad.Eff
import Data.Traversable
import Data.Array
import Math

import Graphics.Canvas hiding (translate)

type Point = { x :: Number, y :: Number }

renderPath :: forall eff. Context2D -> [Point] -> Eff (canvas :: Canvas | eff) Context2D
renderPath ctx (firstPoint : points) = do
  setStrokeStyle "#000000" ctx
  strokePath ctx $ do
    moveTo ctx firstPoint.x firstPoint.y
    traverse (\point -> lineTo ctx point.x point.y) points
    closePath ctx

f :: Number -> Point
f x = { x: x * 4, y: 250 + (sin x * 100) }

main = do
  canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle "#0000FF" ctx

  fillPath ctx $ do
    rect ctx  { x: 250
              , y: 250
              , w: 100
              , h: 100
              }
    rect ctx  { x: 100
              , y: 250
              , w: 100
              , h: 100
              }

  renderPath ctx [{ x: 10, y: 20 }, { x: 30, y: 49 }, { x: 10, y: 60 }]

  renderPath ctx $ f <$> 1 .. 500
