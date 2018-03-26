module ChordMarkup where

import Prelude
import Data.Array
import Data.Maybe
import Graphics.Canvas
import Halogen (liftAff)
import Halogen as H
import Halogen.Aff (selectElement)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Node.FS
import DOM
import DOM.HTML.Types
import DOM.HTML.HTMLElement
import DOM.Event.MouseEvent as ME
import DOM.Event.Types as Event
import DOM.Node.ParentNode (QuerySelector(..))
import Network.HTTP.Affjax
import Music

type ChordMarkup =
  { root :: String
  , qual :: String
  , head_ext :: String
  , tail_ext :: Array String
  }

markup :: Chord -> ChordMarkup
markup (Chord root qual exts) = case uncons (map show exts) of
  Just {head: x, tail: xs} -> mk_exts x xs
  Nothing                  -> mk_exts "" []
  where mk_exts x y = {root:show root, qual:show qual, head_ext:x, tail_ext:y}

display_chord :: forall p i. Chord -> H.HTML p i
display_chord chord =
  HH.span [ HP.id_ "chord-display" ]
    [ HH.span [ HP.class_ (H.ClassName "root") ] [ HH.text (m.root) ]
    , HH.span [ HP.class_ (H.ClassName "mode") ] [ HH.text (m.qual) ]
    , HH.span [ HP.class_ (H.ClassName "head-ext") ] [ HH.text (m.head_ext) ]
    , HH.span_ (map ext_html m.tail_ext)
    ]
  where
    m = markup chord

    ext_html :: String -> H.HTML p i
    ext_html ext = HH.sup
      [ HP.class_ (H.ClassName "extension")]
      [ HH.text $ "("<>ext<>")" ]