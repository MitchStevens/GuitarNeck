"use strict";
var CanvasOperations = require("../CanvasOperations");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Aff_AVar = require("../Control.Monad.Aff.AVar");
var Control_Monad_Aff_Class = require("../Control.Monad.Aff.Class");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Eff_Ref = require("../Control.Monad.Eff.Ref");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var DOM = require("../DOM");
var DOM_Event_MouseEvent = require("../DOM.Event.MouseEvent");
var DOM_Event_Types = require("../DOM.Event.Types");
var DOM_HTML_HTMLElement = require("../DOM.HTML.HTMLElement");
var DOM_HTML_Types = require("../DOM.HTML.Types");
var DOM_Node_ParentNode = require("../DOM.Node.ParentNode");
var Data_Array = require("../Data.Array");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Int = require("../Data.Int");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Record_ShowRecord = require("../Data.Record.ShowRecord");
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var Data_StrMap = require("../Data.StrMap");
var Data_Tuple = require("../Data.Tuple");
var Data_Unit = require("../Data.Unit");
var Debug_Trace = require("../Debug.Trace");
var Fingering = require("../Fingering");
var Fret = require("../Fret");
var Graphics_Canvas = require("../Graphics.Canvas");
var Halogen = require("../Halogen");
var Halogen_Aff = require("../Halogen.Aff");
var Halogen_Aff_Util = require("../Halogen.Aff.Util");
var Halogen_Component = require("../Halogen.Component");
var Halogen_HTML = require("../Halogen.HTML");
var Halogen_HTML_Elements = require("../Halogen.HTML.Elements");
var Halogen_HTML_Events = require("../Halogen.HTML.Events");
var Halogen_HTML_Properties = require("../Halogen.HTML.Properties");
var Halogen_Query_HalogenM = require("../Halogen.Query.HalogenM");
var $$Math = require("../Math");
var Music = require("../Music");
var Music_Chord = require("../Music.Chord");
var Music_Extension = require("../Music.Extension");
var Music_Mode = require("../Music.Mode");
var Music_Note = require("../Music.Note");
var NeckData = require("../NeckData");
var Network_HTTP_Affjax = require("../Network.HTTP.Affjax");
var Node_FS = require("../Node.FS");
var Prelude = require("../Prelude");
var Reader = require("../Reader");
var UI_ChordInput = require("../UI.ChordInput");
var ChordInputSlot = (function () {
    function ChordInputSlot() {

    };
    ChordInputSlot.value = new ChordInputSlot();
    return ChordInputSlot;
})();
var PaintNeck = (function () {
    function PaintNeck(value0) {
        this.value0 = value0;
    };
    PaintNeck.create = function (value0) {
        return new PaintNeck(value0);
    };
    return PaintNeck;
})();
var SetNeck = (function () {
    function SetNeck(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    SetNeck.create = function (value0) {
        return function (value1) {
            return new SetNeck(value0, value1);
        };
    };
    return SetNeck;
})();
var WipeNeck = (function () {
    function WipeNeck(value0) {
        this.value0 = value0;
    };
    WipeNeck.create = function (value0) {
        return new WipeNeck(value0);
    };
    return WipeNeck;
})();
var ClearAll = (function () {
    function ClearAll(value0) {
        this.value0 = value0;
    };
    ClearAll.create = function (value0) {
        return new ClearAll(value0);
    };
    return ClearAll;
})();
var MouseMove = (function () {
    function MouseMove(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    MouseMove.create = function (value0) {
        return function (value1) {
            return new MouseMove(value0, value1);
        };
    };
    return MouseMove;
})();
var SetChord = (function () {
    function SetChord(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    SetChord.create = function (value0) {
        return function (value1) {
            return new SetChord(value0, value1);
        };
    };
    return SetChord;
})();
var ChordInputMessage = (function () {
    function ChordInputMessage(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ChordInputMessage.create = function (value0) {
        return function (value1) {
            return new ChordInputMessage(value0, value1);
        };
    };
    return ChordInputMessage;
})();
var Unit = (function () {
    function Unit() {

    };
    Unit.value = new Unit();
    return Unit;
})();
var next_state = function (either) {
    return function (state) {
        if (either instanceof Data_Either.Left) {
            return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)((function () {
                var $20 = {};
                for (var $21 in state) {
                    if ({}.hasOwnProperty.call(state, $21)) {
                        $20[$21] = state[$21];
                    };
                };
                $20.curr_chord = Data_Maybe.Nothing.value;
                $20.curr_fingerings = [  ];
                $20.curr_focused = Data_Maybe.Nothing.value;
                return $20;
            })());
        };
        if (either instanceof Data_Either.Right) {
            var fingerings = Reader.get_fingerings(state.fingering_cache)(either.value0);
            var centeroids = Data_Array.mapMaybe(Fingering.cache_centeroid(state.neck_data))(fingerings);
            return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)((function () {
                var $24 = {};
                for (var $25 in state) {
                    if ({}.hasOwnProperty.call(state, $25)) {
                        $24[$25] = state[$25];
                    };
                };
                $24.curr_chord = new Data_Maybe.Just(either.value0);
                $24.curr_fingerings = centeroids;
                $24.curr_focused = Data_Maybe.Nothing.value;
                return $24;
            })());
        };
        throw new Error("Failed pattern match at UI.GuitarNeck line 166, column 27 - line 177, column 33: " + [ either.constructor.name ]);
    };
};
var eq_slot = new Data_Eq.Eq(function (x) {
    return function (y) {
        return true;
    };
});
var ord_slot = new Data_Ord.Ord(function () {
    return eq_slot;
}, function (x) {
    return function (y) {
        return Data_Ordering.EQ.value;
    };
});
var chord = new Music_Chord.Chord(Music_Note.C.value, Music_Mode.Major.value, [ new Music_Extension.Add(7) ]);
var calc_offset = function (element) {
    return Control_Apply.lift2(Control_Monad_Eff.applyEff)(Fingering.point)(DOM_HTML_HTMLElement.offsetLeft(element))(DOM_HTML_HTMLElement.offsetTop(element));
};
var guitar_neck = function (neck) {
    var render = function (state) {
        var width = state.neck_data.width + state.neck_data.x_offset;
        var height = state.neck_data.height + state.neck_data.y_offset;
        return Halogen_HTML_Elements.div_([ Halogen_HTML_Elements.div([ Halogen_HTML_Properties.id_("both-canvases") ])([ Halogen_HTML_Elements.canvas([ Halogen_HTML_Properties.id_("guitar_neck"), Halogen_HTML_Properties.width(Data_Int.ceil(width)), Halogen_HTML_Properties.height(Data_Int.ceil(height)) ]), Halogen_HTML_Elements.canvas([ Halogen_HTML_Properties.id_("guitar_notes"), Halogen_HTML_Properties.width(Data_Int.ceil(width)), Halogen_HTML_Properties.height(Data_Int.ceil(height)), Halogen_HTML_Events.onMouseMove(Halogen_HTML_Events.input(MouseMove.create)) ]) ]), Halogen_HTML.slot(ChordInputSlot.value)(UI_ChordInput.chord_input)(Data_Unit.unit)(Halogen_HTML_Events.input(ChordInputMessage.create)) ]);
    };
    var initialState = {
        neck_data: neck,
        curr_chord: Data_Maybe.Nothing.value,
        curr_fingerings: [  ],
        curr_focused: Data_Maybe.Nothing.value,
        fingering_cache: {
            open: Data_StrMap.empty,
            moveable: Data_StrMap.empty
        }
    };
    var $$eval = function (v) {
        if (v instanceof PaintNeck) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v1) {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_Aff_Class.liftAff(Halogen_Query_HalogenM.monadAffHalogenM(Control_Monad_Aff_Class.monadAffAff))(Reader.read_fingerings))(function (v2) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_Eff_Class.liftEff(Halogen_Query_HalogenM.monadEffHalogenM(Control_Monad_Aff.monadEffAff))(CanvasOperations.paint_neck(v1.neck_data)))(function () {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.put(Halogen_Query_HalogenM.monadStateHalogenM)((function () {
                            var $35 = {};
                            for (var $36 in v1) {
                                if ({}.hasOwnProperty.call(v1, $36)) {
                                    $35[$36] = v1[$36];
                                };
                            };
                            $35.fingering_cache = v2;
                            return $35;
                        })()))(function () {
                            return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(v.value0);
                        });
                    });
                });
            });
        };
        if (v instanceof SetNeck) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v1) {
                var new_state = (function () {
                    var $40 = {};
                    for (var $41 in v1) {
                        if ({}.hasOwnProperty.call(v1, $41)) {
                            $40[$41] = v1[$41];
                        };
                    };
                    $40.neck_data = v.value0;
                    return $40;
                })();
                return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_Eff_Class.liftEff(Halogen_Query_HalogenM.monadEffHalogenM(Control_Monad_Aff.monadEffAff))(CanvasOperations.paint_neck(v.value0)))(function () {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.put(Halogen_Query_HalogenM.monadStateHalogenM)(new_state))(function () {
                        return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(v.value1);
                    });
                });
            });
        };
        if (v instanceof WipeNeck) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_Eff_Class.liftEff(Halogen_Query_HalogenM.monadEffHalogenM(Control_Monad_Aff.monadEffAff))(CanvasOperations.wipe_neck))(function () {
                return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(v.value0);
            });
        };
        if (v instanceof ClearAll) {
            return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(v.value0);
        };
        if (v instanceof MouseMove) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v1) {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_Aff_Class.liftAff(Halogen_Query_HalogenM.monadAffHalogenM(Control_Monad_Aff_Class.monadAffAff))(Halogen_Aff_Util.selectElement("#content #guitar")))(function (v2) {
                    return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_Eff_Class.liftEff(Halogen_Query_HalogenM.monadEffHalogenM(Control_Monad_Aff.monadEffAff))(Data_Maybe.maybe(Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Data_Semiring.zero(Fingering.semiring_point)))(calc_offset)(v2)))(function (v3) {
                        var p = Data_Ring.sub(Fingering.ring_point)(Fingering.point(Data_Int.toNumber(DOM_Event_MouseEvent.pageX(v.value0)))(Data_Int.toNumber(DOM_Event_MouseEvent.pageY(v.value0))))(v3);
                        var closest = Fingering.get_closest(Data_Foldable.foldableArray)(v1.neck_data)(p)(v1.curr_fingerings);
                        if (closest instanceof Data_Maybe.Just) {
                            return $$eval(new SetChord(closest.value0.fingering, v.value1));
                        };
                        if (closest instanceof Data_Maybe.Nothing) {
                            return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(v.value1);
                        };
                        throw new Error("Failed pattern match at UI.GuitarNeck line 149, column 7 - line 151, column 29: " + [ closest.constructor.name ]);
                    });
                });
            });
        };
        if (v instanceof SetChord) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v1) {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)($$eval(new WipeNeck(v.value1)))(function (v2) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_Eff_Class.liftEff(Halogen_Query_HalogenM.monadEffHalogenM(Control_Monad_Aff.monadEffAff))(CanvasOperations.paint_chord(v1.neck_data)(v.value0)))(function () {
                        return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(v.value1);
                    });
                });
            });
        };
        if (v instanceof ChordInputMessage) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v1) {
                return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_Aff_Class.liftAff(Halogen_Query_HalogenM.monadAffHalogenM(Control_Monad_Aff_Class.monadAffAff))(next_state(v.value0.value0)(v1)))(function (v2) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.put(Halogen_Query_HalogenM.monadStateHalogenM)(v2))(function () {
                        return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(v.value1);
                    });
                });
            });
        };
        throw new Error("Failed pattern match at UI.GuitarNeck line 126, column 10 - line 163, column 16: " + [ v.constructor.name ]);
    };
    return Halogen_Component.parentComponent(ord_slot)({
        initialState: Data_Function["const"](initialState),
        render: render,
        "eval": $$eval,
        receiver: Data_Function["const"](Data_Maybe.Nothing.value)
    });
};
module.exports = {
    ChordInputSlot: ChordInputSlot,
    PaintNeck: PaintNeck,
    SetNeck: SetNeck,
    WipeNeck: WipeNeck,
    ClearAll: ClearAll,
    MouseMove: MouseMove,
    SetChord: SetChord,
    ChordInputMessage: ChordInputMessage,
    Unit: Unit,
    guitar_neck: guitar_neck,
    next_state: next_state,
    calc_offset: calc_offset,
    chord: chord,
    eq_slot: eq_slot,
    ord_slot: ord_slot
};