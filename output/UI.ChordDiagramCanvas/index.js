"use strict";
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Bifunctor = require("../Data.Bifunctor");
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
var Data_Tuple = require("../Data.Tuple");
var Fingering = require("../Fingering");
var Fret = require("../Fret");
var Graphics_Canvas = require("../Graphics.Canvas");
var Halogen_HTML = require("../Halogen.HTML");
var Interval = require("../Interval");
var Partial_Unsafe = require("../Partial.Unsafe");
var Point = require("../Point");
var Prelude = require("../Prelude");
var UI_CanvasUtils = require("../UI.CanvasUtils");
var v_margin = 0.15;
var num_strings = 6;
var num_frets = 4;
var font_size = 16;
var dot_t = function (v) {
    return new Data_Tuple.Tuple(5.0 - v.value1, v.value0 - 0.5);
};
var dot_shift_t = function (fingering) {
    var min_fret = Interval.inf(Fingering.fret_interval(fingering));
    var m = Data_Maybe.maybe(0.0)(function ($40) {
        return Data_Int.toNumber(Fret.unfret($40));
    })(min_fret);
    var $16 = m <= 1.0;
    if ($16) {
        return dot_t;
    };
    return function ($41) {
        return Data_Bifunctor.rmap(Data_Tuple.bifunctorTuple)(function (v) {
            return v - (m - 1.0);
        })(dot_t($41));
    };
};
var canvas_t = function (v) {
    return function (v1) {
        var alpha = v.height * (1.0 - 2.0 * v_margin) - Data_Int.toNumber(font_size);
        var beta = (v.width - alpha) * 0.5;
        return new Data_Tuple.Tuple((alpha * v1.value0) / Data_Int.toNumber(num_strings - 1 | 0) + beta, (alpha * v1.value1) / Data_Int.toNumber(num_frets) + v_margin + Data_Int.toNumber(font_size));
    };
};
var paint_chord_diagram = function (dims) {
    return function (fingering) {
        return function (id) {
            var paint_point = function (r) {
                return function (ctx) {
                    return function (v) {
                        return Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Graphics_Canvas.beginPath(ctx))(Graphics_Canvas.arc(ctx)(UI_CanvasUtils.circle({
                            x: v.value0,
                            y: v.value1,
                            r: r
                        }))))(Graphics_Canvas.fill(ctx));
                    };
                };
            };
            var paint_line = function (ctx) {
                return function (v) {
                    return Graphics_Canvas.strokePath(ctx)(Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Graphics_Canvas.moveTo(ctx)(Data_Lens_Getter.viewOn(v.value0)(Point._x(Data_Lens_Internal_Forget.strongForget)))(Data_Lens_Getter.viewOn(v.value0)(Point._y(Data_Lens_Internal_Forget.strongForget))))(Graphics_Canvas.lineTo(ctx)(Data_Lens_Getter.viewOn(v.value1)(Point._x(Data_Lens_Internal_Forget.strongForget)))(Data_Lens_Getter.viewOn(v.value1)(Point._y(Data_Lens_Internal_Forget.strongForget)))))(Graphics_Canvas.closePath(ctx)));
                };
            };
            var paint_grid = function (f) {
                return function (ctx) {
                    var v = new Data_Tuple.Tuple(0, 0);
                    var v1 = new Data_Tuple.Tuple(num_strings - 1 | 0, num_frets);
                    var vrline = function (x) {
                        return new Data_Tuple.Tuple(Point.point_int(x)(v.value1), Point.point_int(x)(v1.value1));
                    };
                    var hrline = function (y) {
                        return new Data_Tuple.Tuple(Point.point_int(v.value0)(y), Point.point_int(v1.value0)(y));
                    };
                    var lines = Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Functor.map(Data_Functor.functorArray)(hrline)(Data_Array.range(v.value1)(v1.value1)))(Data_Functor.map(Data_Functor.functorArray)(vrline)(Data_Array.range(v.value0)(v1.value0)));
                    return Data_Array.foldM(Control_Monad_Eff.monadEff)(paint_line)(ctx)(Data_Functor.map(Data_Functor.functorArray)(Data_Bifunctor.bimap(Data_Tuple.bifunctorTuple)(f)(f))(lines));
                };
            };
            var paint_dots = function (f) {
                return function (r) {
                    return function (ctx) {
                        var dots = Data_Functor.map(Data_Functor.functorArray)(function ($42) {
                            return f(dot_shift_t(fingering)($42));
                        })(Fingering.to_points(fingering));
                        return Data_Array.foldM(Control_Monad_Eff.monadEff)(paint_point(r))(ctx)(dots);
                    };
                };
            };
            return Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
                var v = Graphics_Canvas.getCanvasElementById(id)();
                var __unused = function (dictPartial1) {
                    return function ($dollar8) {
                        return $dollar8;
                    };
                };
                return __unused()((function () {
                    if (v instanceof Data_Maybe.Just) {
                        var radius = (Data_Ord.min(Data_Ord.ordNumber)(dims.width)(dims.height) * (1.0 - 2.0 * v_margin)) / 12.0;
                        var f = canvas_t(dims);
                        return Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Graphics_Canvas.getContext2D(v.value0))(paint_grid(f)))(paint_dots(f)(radius));
                    };
                    throw new Error("Failed pattern match at UI.ChordDiagramCanvas line 54, column 3 - line 54, column 41: " + [ v.constructor.name ]);
                })())();
            });
        };
    };
};
module.exports = {
    paint_chord_diagram: paint_chord_diagram
};
