module UI.ChordDiagramQueue where
  
import Halogen as H
import Halogen.HTML as HH

import UI.ChordDiagram as CD
import UI.Queue as Q

type Input m = Q.Input CD.Query CD.Input CD.Message m
type Message = Q.Message
type Query   = Q.Query CD.Input CD.Message
type State m = Q.State CD.Query CD.Input CD.Message m

chord_diagram_queue :: forall m. H.Component HH.HTML Query (Input m) Message m
chord_diagram_queue = Q.ui_queue