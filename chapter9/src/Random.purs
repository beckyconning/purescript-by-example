module Main where

import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.DOM
import Debug.Trace

import Graphics.Canvas

drawRandomCircle = do
  canvas <- getCanvasElementById "canvas"
  ctx <- getContext2D canvas

  setFillStyle "purple" ctx
  setStrokeStyle "pink" ctx

  x <- random
  y <- random
  w <- random
  h <- random

  let path = rect ctx
       { x : x * 600
       , y : y * 600
       , w : (w + 2) * 50
       , h : (h + 2) * 50
       }

  fillPath ctx path
  strokePath ctx path

  return unit

rotateAround :: forall eff. { x :: Number, y :: Number } -> Number -> Context2D -> Eff (canvas :: Canvas | eff) Context2D
rotateAround center angle ctx = translate { translateX: 0, translateY: 0 } ctx
  >>= translate { translateX: center.x, translateY: center.y }
  >>= rotate angle

main = body >>= addEventListener "click" (drawRandomCircle >>= \_ -> trace "hi")
