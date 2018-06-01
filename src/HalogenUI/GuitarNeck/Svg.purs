module UI.GuitarNeck.DrawSvg where

import Prelude
import Svg.Attributes

import CSS (Position(..))
import DOM.HTML.Indexed (GlobalAttributes)
import Data.Array (zipWith, (..))
import Data.Int (toNumber)
import Data.Tuple (Tuple(..))
import Halogen (HTML)
import Halogen.HTML as HH
import Halogen.HTML.Properties (IProp(..))
import Halogen.HTML.Properties as HP
import Math (log, pow)
import NeckData (NeckData)
import Point (Point)
import Svg.Elements (circle, g, rect, svg)

num_frets = 15 :: Int
--Fender Strat. neck data
fret_width = 0.2413 :: Number
scale_length = 32.4 :: Number
fingerboard_length :: Number
fingerboard_length =
  2.0*scale_length*(1.0 - pow 0.5 (toNumber num_frets / 12.0))

type Positioned r = ( x :: Number, y :: Number | r)
type ContinuousMapping r i = IProp (Positioned r) i -> IProp (Positioned r) i
continuous :: forall r i. (Number -> Number) -> (Number -> Number) -> ContinuousMapping r i
continuous fx fy p = p { x = fx p.x, y = fy p.y }

draw_neck :: forall p i. NeckData -> HTML p i
draw_neck neck = svg
    [ viewBox 0.0 0.0 neck.width neck.height ]
    [ draw_fretboard
    , draw_frets ]
  where

  {- 
  
  -}
  neck_t :: ContinuousMapping
  neck_t = continuous f g
    where
      f x = p x / p (toNumber num_frets) where p z = 1.0 - pow 2.0 (-x / 12.0)
      g x = x / 6.0

  draw_fretboard :: HTML p i
  draw_fretboard = rect
    [ x neck.x_offset
    , y neck.y_offset
    , height (neck.height - neck.x_offset)
    , width  (neck.width  - neck.y_offset)
    , HP.id_ "fretboard" ]

  draw_frets :: HTML p i
  draw_frets = g
    [ transform
      [ Translate neck.x_offset neck.y_offset
      , Scale
        fingerboard_length
        (neck.height - neck.y_offset) ]
    , HP.id_ "frets"]
    (map (neck_t<<<draw_fret) (1..num_frets))
    where
      draw_fret :: Int -> HTML p i
      draw_fret fret = rect
        [ x (toNumber fret)
        , y 0.0
        , height 1.0
        , width 0.04 ]

      f x = let k = log (0.5) / 12.0
                a = 1
            in
          

  draw_inlays :: HTML p i
  draw_inlays = g
    [ HP.id_ "inlays"
    , transform
      [ Translate neck.x_offset neck.y_offset
      , Scale 1.0 1.0 ]]
    (map single_inlay inlay_points)
    where
      inlay_points = [Tuple 12.0 0.5, Tuple 12.0 4.5] <>
        map ((\x -> Tuple x 2.5) <<< toNumber) [3, 5, 7, 9, 15, 17, 19, 21]

      single_inlay :: Point -> HTML p i
      single_inlay (Tuple x y) = circle
        [ HP.class_ (HH.ClassName "inlay")
        , cx x, cy y ]
    
  draw_strings :: HTML p i
  draw_strings = g
    [ HP.id_ "strings"
    , transform
      [ Translate neck.x_offset neck.y_offset
      , Scale (neck.width - neck.x_offset) (neck.height - neck.y_offset) ] ]
    (zipWith (single_string<<<toNumber) (0..5) gauges)
    where
      gauges = [0.2540, 0.3302, 0.4318, 0.6604, 0.9144, 1.1684]
      fret_size = 50.0
      single_string n gauge = rect
          [ x 0.0
          , y ((2.0*n + 1.0) / 12.0 - (gauge / fret_size) / 2.0)
          , height (gauge / fret_size)
          , width 1.0 ]

{-
  paint_numbers :: HTML p i
  paint_numbers = g
    [ HP.id_ "numbers"
    , transform
      [ Transform  ] ]
    ()
    where
        draw_number :: Int -> HTML p i
        draw_number n =
-}
