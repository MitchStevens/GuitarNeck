module UI.CanvasUtils (circle) where

import Prelude
import Math
import Graphics.Canvas (Arc)

circle :: { x :: Number, y:: Number, r :: Number } -> Arc
circle c = { x: c.x, y: c.y, r: c.r, start: 0.0, end: 2.0 * pi}