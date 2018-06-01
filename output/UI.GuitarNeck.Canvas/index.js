// Generated by purs version 0.11.7
"use strict";
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Free_Trans = require("../Control.Monad.Free.Trans");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Identity = require("../Data.Identity");
var Data_Int = require("../Data.Int");
var Data_Lens = require("../Data.Lens");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Debug_Trace = require("../Debug.Trace");
var Fingering = require("../Fingering");
var Fret = require("../Fret");
var Graphics_Canvas = require("../Graphics.Canvas");
var Graphics_Canvas_Free = require("../Graphics.Canvas.Free");
var $$Math = require("../Math");
var NeckData = require("../NeckData");
var Partial_Unsafe = require("../Partial.Unsafe");
var Point = require("../Point");
var Prelude = require("../Prelude");
var UI_CanvasUtils = require("../UI.CanvasUtils");
var UI_FFTypes = require("../UI.FFTypes");
var UI_GuitarNeck_Types = require("../UI.GuitarNeck.Types");
var wipe_neck = Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
    var v = Graphics_Canvas.getCanvasElementById("guitar-notes")();
    var __unused = function (dictPartial1) {
        return function ($dollar11) {
            return $dollar11;
        };
    };
    return __unused()((function () {
        if (v instanceof Data_Maybe.Just) {
            return function __do() {
                var v1 = Graphics_Canvas.getContext2D(v.value0)();
                var v2 = Graphics_Canvas.getCanvasDimensions(v.value0)();
                return Graphics_Canvas.clearRect(v1)({
                    x: 0.0,
                    y: 0.0,
                    w: v2.width,
                    h: v2.height
                })();
            };
        };
        throw new Error("Failed pattern match at UI.GuitarNeck.Canvas line 170, column 3 - line 171, column 3: " + [ v.constructor.name ]);
    })())();
});
var white = "#FFFFFF";
var transparent = "transparent";
var string_color = "#dddddd";
var neck_color = "#edc889";
var inlay_radius = 5.0;
var inlay_color = "#dddddd";
var fret_width = 2.0;
var fret_color = "#000000";
var font_size = 14;
var paint_neck = function (neck) {
    var paint_fretboard = Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Graphics_Canvas_Free.setFillStyle(Data_Identity.monadIdentity)(neck_color))(function () {
        return Graphics_Canvas_Free.fillRect(Data_Identity.monadIdentity)({
            x: neck.x_offset,
            y: neck.y_offset,
            w: neck.width,
            h: neck.height
        });
    });
    var neck_t = NeckData.neck_transformation(neck);
    var paint_strings = (function () {
        var gauges = [ 0.254, 0.3302, 0.4318, 0.6604, 0.9144, 1.1684 ];
        var single_string = function (n) {
            return function (g) {
                var v = neck_t(Point.point_int(0)(n));
                return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Graphics_Canvas_Free.beginPath(Data_Identity.monadIdentity))(function () {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Graphics_Canvas_Free.fillRect(Data_Identity.monadIdentity)({
                        x: v.value0,
                        y: v.value1,
                        w: neck.width,
                        h: (neck.height * g) / 50.0
                    }))(function () {
                        return Graphics_Canvas_Free.stroke(Data_Identity.monadIdentity);
                    });
                });
            };
        };
        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Graphics_Canvas_Free.setFillStyle(Data_Identity.monadIdentity)(string_color))(function () {
            return Data_Foldable.sequence_(Control_Monad_Free_Trans.applicativeFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Data_Foldable.foldableArray)(Data_Array.zipWith(single_string)(Data_Array.range(0)(5))(gauges));
        });
    })();
    var fret_t = NeckData.fret_transformation(neck);
    var paint_fret = function (fret) {
        var v = Data_Ring.sub(Data_Tuple.ringTuple(Data_Ring.ringNumber)(Data_Ring.ringNumber))(fret_t(new Data_Tuple.Tuple(Data_Int.toNumber(fret), -0.5)))(new Data_Tuple.Tuple(0.0, fret_width * 0.5));
        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Graphics_Canvas_Free.setFillStyle(Data_Identity.monadIdentity)(fret_color))(function () {
            return Graphics_Canvas_Free.fillRect(Data_Identity.monadIdentity)({
                x: v.value0,
                y: v.value1,
                w: fret_width,
                h: neck.height
            });
        });
    };
    var paint_frets = Data_Foldable.traverse_(Control_Monad_Free_Trans.applicativeFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Data_Foldable.foldableArray)(paint_fret)(Data_Array.range(0)(neck.num_frets));
    var paint_inlays = (function () {
        var single_inlay = function (v) {
            return Control_Apply.applySecond(Control_Monad_Free_Trans.applyFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Control_Apply.applySecond(Control_Monad_Free_Trans.applyFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Graphics_Canvas_Free.beginPath(Data_Identity.monadIdentity))(Graphics_Canvas_Free.arc(Data_Identity.monadIdentity)(UI_CanvasUtils.circle({
                x: v.value0,
                y: v.value1,
                r: inlay_radius
            }))))(Graphics_Canvas_Free.fill(Data_Identity.monadIdentity));
        };
        var inlay_points = Data_Semigroup.append(Data_Semigroup.semigroupArray)([ new Data_Tuple.Tuple(12.0, 0.5), new Data_Tuple.Tuple(12.0, 4.5) ])(Data_Functor.map(Data_Functor.functorArray)(function ($58) {
            return Data_Function.flip(Data_Tuple.Tuple.create)(2.5)(Data_Int.toNumber($58));
        })([ 3, 5, 7, 9, 15, 17, 19, 21 ]));
        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Graphics_Canvas_Free.setFillStyle(Data_Identity.monadIdentity)(inlay_color))(function () {
            return Data_Foldable.traverse_(Control_Monad_Free_Trans.applicativeFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Data_Foldable.foldableArray)(function ($59) {
                return single_inlay(fret_t($59));
            })(inlay_points);
        });
    })();
    var paint_numbers = (function () {
        var paint_number = function (num) {
            return Control_Bind.bind(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Graphics_Canvas_Free.measureText(Data_Identity.monadIdentity)(Data_Show.show(Data_Show.showInt)(num)))(function (v) {
                var v1 = Data_Ring.sub(Data_Tuple.ringTuple(Data_Ring.ringNumber)(Data_Ring.ringNumber))(fret_t(new Data_Tuple.Tuple(Data_Int.toNumber(num), 0.0)))(new Data_Tuple.Tuple(v.width * 0.5, neck.y_offset));
                return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Graphics_Canvas_Free.setFont(Data_Identity.monadIdentity)(Data_Show.show(Data_Show.showInt)(font_size) + "px Georgia"))(function () {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Graphics_Canvas_Free.setFillStyle(Data_Identity.monadIdentity)(white))(function () {
                        return Graphics_Canvas_Free.fillText(Data_Identity.monadIdentity)(Data_Show.show(Data_Show.showInt)(num))(v1.value0)(Data_Ord.max(Data_Ord.ordNumber)(0.0)(v1.value1));
                    });
                });
            });
        };
        return Data_Foldable.traverse_(Control_Monad_Free_Trans.applicativeFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Data_Foldable.foldableArray)(paint_number)(Data_Array.range(1)(neck.num_frets));
    })();
    return Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
        var v = Graphics_Canvas.getCanvasElementById("guitar-neck")();
        var __unused = function (dictPartial1) {
            return function ($dollar15) {
                return $dollar15;
            };
        };
        return __unused()((function () {
            if (v instanceof Data_Maybe.Just) {
                return function __do() {
                    var v1 = Graphics_Canvas.getContext2D(v.value0)();
                    return Graphics_Canvas_Free.runGraphics(v1)(Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(paint_fretboard)(function () {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(paint_frets)(function () {
                            return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(paint_inlays)(function () {
                                return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(paint_strings)(function () {
                                    return paint_numbers;
                                });
                            });
                        });
                    }))();
                };
            };
            throw new Error("Failed pattern match at UI.GuitarNeck.Canvas line 44, column 3 - line 45, column 3: " + [ v.constructor.name ]);
        })())();
    });
};
var chord_color = "#ee1122";
var display_neck = function (state) {
    var paint_centeroid = function (fing) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Graphics_Canvas_Free.beginPath(Data_Identity.monadIdentity))(function () {
            return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Graphics_Canvas_Free.arc(Data_Identity.monadIdentity)(UI_CanvasUtils.circle({
                x: fing.centeroid.value0,
                y: fing.centeroid.value1,
                r: 40.0
            })))(function () {
                return Graphics_Canvas_Free.fill(Data_Identity.monadIdentity);
            });
        });
    };
    var fret_t = NeckData.fret_transformation(state.neck_data);
    var paint_chord = function (fing) {
        var paint_dot = function (v) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Graphics_Canvas_Free.beginPath(Data_Identity.monadIdentity))(function () {
                return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Graphics_Canvas_Free.setFillStyle(Data_Identity.monadIdentity)(chord_color))(function () {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Graphics_Canvas_Free.arc(Data_Identity.monadIdentity)(UI_CanvasUtils.circle({
                        x: v.value0,
                        y: v.value1,
                        r: 8.5
                    })))(function () {
                        return Graphics_Canvas_Free.fill(Data_Identity.monadIdentity);
                    });
                });
            });
        };
        return Data_Foldable.traverse_(Control_Monad_Free_Trans.applicativeFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Data_Foldable.foldableArray)(paint_dot)(Data_Functor.map(Data_Functor.functorArray)(fret_t)(Fingering.to_points(fing.fingering)));
    };
    var paint_fingerings = (function () {
        var paint_fingering = function (fing) {
            return function (n) {
                var $54 = Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqInt))(state.focused)(new Data_Maybe.Just(n));
                if ($54) {
                    return paint_chord(fing);
                };
                return paint_centeroid(fing);
            };
        };
        return Data_Foldable.sequence_(Control_Monad_Free_Trans.applicativeFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Data_Foldable.foldableArray)(Data_Array.zipWith(paint_fingering)(state.curr_fingerings)(Data_Array.range(0)(Data_Array.length(state.curr_fingerings))));
    })();
    return Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
        var v = Graphics_Canvas.getCanvasElementById("guitar-notes")();
        var __unused = function (dictPartial1) {
            return function ($dollar19) {
                return $dollar19;
            };
        };
        return __unused()((function () {
            if (v instanceof Data_Maybe.Just) {
                return function __do() {
                    var v1 = Graphics_Canvas.getContext2D(v.value0)();
                    return Graphics_Canvas_Free.runGraphics(v1)(Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Free_Trans.bindFreeT(Graphics_Canvas_Free.functorGraphicsF)(Data_Identity.monadIdentity))(Graphics_Canvas_Free.setFillStyle(Data_Identity.monadIdentity)(chord_color))(function () {
                        return paint_fingerings;
                    }))();
                };
            };
            throw new Error("Failed pattern match at UI.GuitarNeck.Canvas line 114, column 3 - line 115, column 3: " + [ v.constructor.name ]);
        })())();
    });
};
module.exports = {
    paint_neck: paint_neck,
    display_neck: display_neck,
    wipe_neck: wipe_neck
};
