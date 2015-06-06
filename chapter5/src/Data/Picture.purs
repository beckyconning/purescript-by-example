module Data.Picture where

import Data.Foldable
import Data.Array
import Data.Maybe
import Math

data Point = Point 
  { x :: Number
  , y :: Number 
  }

showPoint :: Point -> String
showPoint (Point { x = x, y = y }) = 
  "(" ++ show x ++ ", " ++ show y ++ ")"

data Shape 
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String
  | Clipped Point Picture

instance showShape :: Show Shape where
  show (Circle c r) = 
    "Circle [center: " ++ showPoint c ++ ", radius: " ++ show r ++ "]"
  show (Rectangle c w h) = 
    "Rectangle [center: " ++ showPoint c ++ ", width: " ++ show w ++ ", height: " ++ show h ++ "]"
  show (Line start end) = 
    "Line [start: " ++ showPoint start ++ ", end: " ++ showPoint end ++ "]"
  show (Text loc text) = 
    "Text [location: " ++ showPoint loc ++ ", text: " ++ show text ++ "]"

type Picture = [Shape]

showPicture :: Picture -> String
showPicture picture = "[" ++ go picture ++ "]"
  where
  go :: Picture -> String
  go [] = ""
  go [x] = show x
  go (x : xs) = show x ++ ", " ++ go xs

data Bounds = Bounds
  { top    :: Number
  , left   :: Number
  , bottom :: Number
  , right  :: Number 
  }

showBounds :: Bounds -> String
showBounds (Bounds b) = 
  "Bounds [top: " ++ show b.top ++
  ", left: "      ++ show b.left ++
  ", bottom: "    ++ show b.bottom ++
  ", right: "     ++ show b.right ++
  "]"

shapeBounds :: Shape -> Bounds
shapeBounds (Circle (Point { x = x, y = y }) r) = Bounds
  { top:    y - r
  , left:   x - r
  , bottom: y + r
  , right:  x + r
  }
shapeBounds (Rectangle (Point { x = x, y = y }) w h) = Bounds
  { top:    y - h / 2
  , left:   x - w / 2
  , bottom: y + h / 2
  , right:  x + w / 2
  }
shapeBounds (Line (Point p1) (Point p2)) = Bounds
  { top:    Math.min p1.y p2.y
  , left:   Math.min p1.x p2.x
  , bottom: Math.max p1.y p2.y
  , right:  Math.max p1.x p2.x
  }
shapeBounds (Text (Point { x = x, y = y }) _) = Bounds
  { top:    y
  , left:   x
  , bottom: y
  , right:  x
  }
shapeBounds (Clipped _ picture) = bounds picture

(\/) :: Bounds -> Bounds -> Bounds
(\/) (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.min b1.top    b2.top
  , left:   Math.min b1.left   b2.left
  , bottom: Math.max b1.bottom b2.bottom
  , right:  Math.max b1.right  b2.right
  }

(/\) :: Bounds -> Bounds -> Bounds
(/\) (Bounds b1) (Bounds b2) = Bounds
  { top:    Math.max b1.top    b2.top
  , left:   Math.max b1.left   b2.left
  , bottom: Math.min b1.bottom b2.bottom
  , right:  Math.min b1.right  b2.right
  }

emptyBounds :: Bounds
emptyBounds = Bounds
  { top:     Global.infinity
  , left:    Global.infinity
  , bottom: -Global.infinity
  , right:  -Global.infinity
  }

infiniteBounds :: Bounds
infiniteBounds = Bounds
  { top:    -Global.infinity
  , left:   -Global.infinity
  , bottom:  Global.infinity
  , right:   Global.infinity
  }

bounds :: Picture -> Bounds
bounds = foldl combine emptyBounds
  where
  combine :: Bounds -> Shape -> Bounds
  combine b shape = shapeBounds shape \/ b

gcd :: Number -> Number -> Number
gcd n 0 = n
gcd 0 m = m
gcd n m = if n > m then gcd (n - m) m else gcd n (m - n)

gcd' :: Number -> Number -> Number
gcd' n 0 = n
gcd' 0 m = m
gcd' n m | n > m = gcd (n - m) m
gcd' n m         = gcd n (m - n)

factorial :: Number -> Number
factorial 0 = 1
factorial n = n * (factorial (n - 1))

pascal :: Number -> Number -> Number
pascal _ k | k < 1 = 1
pascal n k | k > n = 0
pascal n k = (pascal (n - 1) k) + (pascal (n - 1) (k - 1))

allTrue :: [Boolean] -> Boolean
allTrue [] = true
allTrue (x : xs) = x && allTrue xs

isSorted :: [Number] -> Boolean
isSorted [] = true
isSorted [_] = true
isSorted (x : y : zs) = x < y && isSorted (y : zs)

getCity :: forall a b. { address :: { city :: String | b } | a } -> String
getCity x = x.address.city

flatten :: forall a. [[a]] -> [a]
flatten [] = []
flatten [x] = x
flatten (x : y : zs) = x ++ y ++ (flatten zs)

aCircle :: Shape
aCircle = Circle (Point { x: 0, y: 0 }) 10

scaleByTwo :: Shape -> Shape
scaleByTwo (Circle position radius) = Circle position (radius * 2)
scaleByTwo (Rectangle position width height) = Rectangle position (width * 2) (height * 2)
scaleByTwo (Line (Point start) (Point end)) = Line scaledStart scaledEnd
  where
  difference = { x: end.x - start.x, y: end.y - start.y }
  scaledStart = Point { x: start.x - (difference.x / 2), y: start.y - (difference.y / 2) }
  scaledEnd = Point { x: end.x + (difference.x / 2), y: end.y + (difference.y / 2) }
scaleByTwo text@(Text _ _) = text

shapeText :: Shape -> Maybe String
shapeText (Text _ text) = Just text
shapeText _ = Nothing

area :: Shape -> Number
area (Circle _ radius) = 2 * (pi * (radius `pow` 2))
area (Rectangle _ width height) = width * height
are _ = 0
