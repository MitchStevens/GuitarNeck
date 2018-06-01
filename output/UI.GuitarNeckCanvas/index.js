"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Int = require("../Data.Int");
var Data_Lens = require("../Data.Lens");
var Data_Lens_Getter = require("../Data.Lens.Getter");
var Data_Lens_Internal_Forget = require("../Data.Lens.Internal.Forget");
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
var $$Math = require("../Math");
var NeckData = require("../NeckData");
var Partial_Unsafe = require("../Partial.Unsafe");
var Point = require("../Point");
var Prelude = require("../Prelude");
var UI_CanvasUtils = require("../UI.CanvasUtils");
var UI_FFTypes = require("../UI.FFTypes");
var wipe_neck = Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
    var v = Graphics_Canvas.getCanvasElementById("guitar_notes")();
    var __unused = function (dictPartial1) {
        return function ($dollar10) {
            return $dollar10;
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
        throw new Error("Failed pattern match at UI.GuitarNeckCanvas line 158, column 3 - line 159, column 3: " + [ v.constructor.name ]);
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
    var neck_t = NeckData.neck_transformation(neck);
    var paint_strings = function (ctx) {
        var gauges = [ 0.254, 0.3302, 0.4318, 0.6604, 0.9144, 1.1684 ];
        var single_string = function (n) {
            return function (g) {
                var p = neck_t(Point.point_int(0)(n));
                return Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Graphics_Canvas.beginPath(ctx))(Graphics_Canvas.fillRect(ctx)({
                    x: Data_Lens_Getter.viewOn(p)(Point._x(Data_Lens_Internal_Forget.strongForget)),
                    y: Data_Lens_Getter.viewOn(p)(Point._y(Data_Lens_Internal_Forget.strongForget)),
                    w: neck.width,
                    h: (neck.height * g) / 50.0
                }));
            };
        };
        return function __do() {
            Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.setFillStyle(string_color)(ctx))();
            Data_Foldable.sequence_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)(Data_Array.zipWith(single_string)(Data_Array.range(0)(5))(gauges))();
            return ctx;
        };
    };
    var fret_t = NeckData.fret_transformation(neck);
    var paint_fret = function (ctx) {
        return function (fret) {
            var v = Data_Ring.sub(Data_Tuple.ringTuple(Data_Ring.ringNumber)(Data_Ring.ringNumber))(fret_t(new Data_Tuple.Tuple(Data_Int.toNumber(fret), -0.5)))(new Data_Tuple.Tuple(0.0, fret_width * 0.5));
            return Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Graphics_Canvas.setFillStyle(fret_color)(ctx))(Graphics_Canvas.fillRect(ctx)({
                x: v.value0,
                y: v.value1,
                w: fret_width,
                h: neck.height
            }));
        };
    };
    var paint_frets = function (ctx) {
        return Data_Array.foldM(Control_Monad_Eff.monadEff)(paint_fret)(ctx)(Data_Array.range(0)(neck.num_frets));
    };
    var paint_inlays = function (ctx) {
        var single_inlay = function (p) {
            return Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Graphics_Canvas.beginPath(ctx))(Data_Function.flip(Graphics_Canvas.arc)(UI_CanvasUtils.circle({
                x: Data_Lens_Getter.viewOn(p)(Point._x(Data_Lens_Internal_Forget.strongForget)),
                y: Data_Lens_Getter.viewOn(p)(Point._y(Data_Lens_Internal_Forget.strongForget)),
                r: inlay_radius
            }))))(Graphics_Canvas.fill));
        };
        var inlay_points = Data_Semigroup.append(Data_Semigroup.semigroupArray)([ new Data_Tuple.Tuple(12.0, 0.5), new Data_Tuple.Tuple(12.0, 4.5) ])(Data_Functor.map(Data_Functor.functorArray)(function ($52) {
            return Data_Function.flip(Data_Tuple.Tuple.create)(2.5)(Data_Int.toNumber($52));
        })([ 3, 5, 7, 9, 15, 17, 19, 21 ]));
        return Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Graphics_Canvas.setFillStyle(inlay_color)(ctx))(Data_Foldable.traverse_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)(single_inlay)(Data_Functor.map(Data_Functor.functorArray)(fret_t)(inlay_points))))(Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(ctx));
    };
    var paint_numbers = function (ctx) {
        var paint_number = function (ctx1) {
            return function (num) {
                return function __do() {
                    var v = Graphics_Canvas.measureText(ctx1)(Data_Show.show(Data_Show.showInt)(num))();
                    var v1 = Data_Ring.sub(Data_Tuple.ringTuple(Data_Ring.ringNumber)(Data_Ring.ringNumber))(fret_t(new Data_Tuple.Tuple(Data_Int.toNumber(num), 0.0)))(new Data_Tuple.Tuple(v.width * 0.5, neck.y_offset));
                    return Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Graphics_Canvas.setFont(Data_Show.show(Data_Show.showInt)(font_size) + "px Georgia")(ctx1))(Graphics_Canvas.setFillStyle(white)(ctx1)))(Graphics_Canvas.fillText(ctx1)(Data_Show.show(Data_Show.showInt)(num))(v1.value0)(Data_Ord.max(Data_Ord.ordNumber)(0.0)(v1.value1)))();
                };
            };
        };
        return Data_Array.foldM(Control_Monad_Eff.monadEff)(paint_number)(ctx)(Data_Array.range(1)(neck.num_frets));
    };
    return Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(function __do() {
        var v = Graphics_Canvas.getCanvasElementById("guitar_neck")();
        var __unused = function (dictPartial1) {
            return function ($dollar14) {
                return $dollar14;
            };
        };
        return __unused()((function () {
            if (v instanceof Data_Maybe.Just) {
                return Graphics_Canvas.getContext2D(v.value0);
            };
            throw new Error("Failed pattern match at UI.GuitarNeckCanvas line 42, column 3 - line 43, column 3: " + [ v.constructor.name ]);
        })())();
    })(Graphics_Canvas.setFillStyle(neck_color)))(Data_Function.flip(Graphics_Canvas.fillRect)({
        x: neck.x_offset,
        y: neck.y_offset,
        w: neck.width,
        h: neck.height
    })))(paint_frets))(paint_inlays))(paint_strings))(paint_numbers));
};
var chord_color = "#ee1122";
var display_neck = function (state) {
    var paint_centeroid = function (fing) {
        return function (ctx) {
            return function __do() {
                var v = Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Graphics_Canvas.createRadialGradient({
                    x0: fing.centeroid.value0,
                    y0: fing.centeroid.value1,
                    r0: 0.0,
                    x1: fing.centeroid.value0,
                    y1: fing.centeroid.value1,
                    r1: 40.0
                })(ctx))(Graphics_Canvas.addColorStop(0.0)(chord_color)))(Graphics_Canvas.addColorStop(1.0)(transparent))();
                return Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Graphics_Canvas.beginPath(ctx))(Graphics_Canvas.setGradientFillStyle(v)))(Data_Function.flip(Graphics_Canvas.arc)(UI_CanvasUtils.circle({
                    x: fing.centeroid.value0,
                    y: fing.centeroid.value1,
                    r: 40.0
                }))))(Graphics_Canvas.fill)();
            };
        };
    };
    var fret_t = NeckData.fret_transformation(state.neck_data);
    var paint_chord = function (fing) {
        return function (ctx) {
            var paint_dot = function (v) {
                return Control_Bind.composeKleisli(Control_Monad_Eff.bindEff)(Graphics_Canvas.beginPath)(Control_Bind.composeKleisli(Control_Monad_Eff.bindEff)(Graphics_Canvas.setFillStyle(chord_color))(Control_Bind.composeKleisli(Control_Monad_Eff.bindEff)(Data_Function.flip(Graphics_Canvas.arc)(UI_CanvasUtils.circle({
                    x: v.value0,
                    y: v.value1,
                    r: 8.5
                })))(Graphics_Canvas.fill)));
            };
            return Data_Array.foldM(Control_Monad_Eff.monadEff)(Data_Function.flip(paint_dot))(ctx)(Data_Functor.map(Data_Functor.functorArray)(fret_t)(Fingering.to_points(fing.fingering)));
        };
    };
    var paint_fingerings = function (ctx) {
        var paint_fingering = function (v) {
            var $47 = Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqInt))(state.focused)(new Data_Maybe.Just(v.value1));
            if ($47) {
                return paint_chord(v.value0);
            };
            return paint_centeroid(v.value0);
        };
        var tuples = Data_Array.zip(state.curr_fingerings)(Data_Array.range(0)(Data_Array.length(state.curr_fingerings)));
        return Data_Array.foldM(Control_Monad_Eff.monadEff)(Data_Function.flip(paint_fingering))(ctx)(tuples);
    };
    return Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(function __do() {
        var v = Graphics_Canvas.getCanvasElementById("guitar_notes")();
        var __unused = function (dictPartial1) {
            return function ($dollar18) {
                return $dollar18;
            };
        };
        return __unused()((function () {
            if (v instanceof Data_Maybe.Just) {
                return Graphics_Canvas.getContext2D(v.value0);
            };
            throw new Error("Failed pattern match at UI.GuitarNeckCanvas line 107, column 3 - line 108, column 3: " + [ v.constructor.name ]);
        })())();
    })(Data_Function.flip(Graphics_Canvas.setGlobalAlpha)(0.5)))(Graphics_Canvas.setFillStyle(chord_color)))(paint_fingerings))(Data_Function.flip(Graphics_Canvas.setGlobalAlpha)(1.0)));
};
module.exports = {
    paint_neck: paint_neck,
    display_neck: display_neck,
    wipe_neck: wipe_neck
};
