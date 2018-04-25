"use strict";
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
var UI_FFTypes = require("../UI.FFTypes");
var wipe_neck = Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
    var v = Graphics_Canvas.getCanvasElementById("guitar_notes")();
    var __unused = function (dictPartial1) {
        return function ($dollar19) {
            return $dollar19;
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
        throw new Error("Failed pattern match at UI.GuitarNeckCanvas line 156, column 3 - line 157, column 3: " + [ v.constructor.name ]);
    })())();
});
var white = "#FFFFFF";
var transparent = "transparent";
var string_color = "#dddddd";
var paint_strings = function (v) {
    var gauges = [ 0.254, 0.3302, 0.4318, 0.6604, 0.9144, 1.1684 ];
    var f = NeckData.neck_transformation(v.value1);
    var single_string = function (n) {
        return function (g) {
            var v1 = f(Point.point_int(0)(n));
            return Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Graphics_Canvas.beginPath(v.value0))(Graphics_Canvas.fillRect(v.value0)({
                x: v1.x,
                y: v1.y,
                w: v.value1.width,
                h: (v.value1.height * g) / 50.0
            }));
        };
    };
    return Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Graphics_Canvas.setFillStyle(string_color)(v.value0))(Data_Foldable.sequence_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)(Data_Array.zipWith(single_string)(Data_Array.range(0)(5))(gauges)));
};
var neck_color = "#edc889";
var inlay_radius = 5.0;
var inlay_color = "#dddddd";
var fret_width = 2.0;
var fret_color = "#000000";
var paint_fret = function (v) {
    return function (fret) {
        var f = NeckData.neck_transformation(v.value1);
        var v1 = Data_Ring.sub(Point.ring_point)(f(Point.point(Data_Int.toNumber(fret))(-0.5)))(Point.point(0.0)(fret_width * 0.5));
        return Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Graphics_Canvas.setFillStyle(fret_color)(v.value0))(Graphics_Canvas.fillRect(v.value0)({
            x: v1.x,
            y: v1.y,
            w: fret_width,
            h: v.value1.height
        })));
    };
};
var font_size = 14;
var paint_number = function (v) {
    return function (num) {
        var write_text = function (str) {
            return function (width) {
                var f = NeckData.fret_transformation(v.value1);
                var v1 = Data_Ring.sub(Point.ring_point)(f(Point.point(Data_Int.toNumber(num))(0.0)))(Point.point(width * 0.5)(v.value1.y_offset));
                return Data_Functor["void"](Control_Monad_Eff.functorEff)(Graphics_Canvas.fillText(v.value0)(str)(v1.x)(Data_Ord.max(Data_Ord.ordNumber)(0.0)(v1.y)));
            };
        };
        return Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
            var v1 = Graphics_Canvas.setFont(Data_Show.show(Data_Show.showInt)(font_size) + "px Georgia")(v.value0)();
            var v2 = Graphics_Canvas.setFillStyle(white)(v.value0)();
            var v3 = Graphics_Canvas.measureText(v.value0)(Data_Show.show(Data_Show.showInt)(num))();
            return write_text(Data_Show.show(Data_Show.showInt)(num))(v3.width)();
        });
    };
};
var circle = function (c) {
    return {
        x: c.x,
        y: c.y,
        r: c.r,
        start: 0.0,
        end: 2.0 * $$Math.pi
    };
};
var paint_inlays = function (v) {
    var single_inlay = function (v1) {
        return Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Graphics_Canvas.beginPath(v.value0))(Data_Function.flip(Graphics_Canvas.arc)(circle({
            x: v1.x,
            y: v1.y,
            r: inlay_radius
        }))))(Graphics_Canvas.fill));
    };
    var inlay_points = Data_Semigroup.append(Data_Semigroup.semigroupArray)([ Point.point(12.0)(0.5), Point.point(12.0)(4.5) ])(Data_Functor.map(Data_Functor.functorArray)(function ($74) {
        return (function (x) {
            return Point.point(x)(2.5);
        })(Data_Int.toNumber($74));
    })([ 3, 5, 7, 9, 15, 17, 19, 21 ]));
    var f = NeckData.fret_transformation(v.value1);
    return Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Graphics_Canvas.setFillStyle(inlay_color)(v.value0))(Data_Foldable.traverse_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)(single_inlay)(Data_Functor.map(Data_Functor.functorArray)(f)(inlay_points))));
};
var paint_neck = function (neck) {
    return Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
        var v = Graphics_Canvas.getCanvasElementById("guitar_neck")();
        var __unused = function (dictPartial1) {
            return function ($dollar23) {
                return $dollar23;
            };
        };
        return __unused()((function () {
            if (v instanceof Data_Maybe.Just) {
                return function __do() {
                    var v1 = Graphics_Canvas.getContext2D(v.value0)();
                    var tuple = new Data_Tuple.Tuple(v1, neck);
                    return Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Graphics_Canvas.setFillStyle(neck_color)(v1))(Graphics_Canvas.fillRect(v1)({
                        x: neck.x_offset,
                        y: neck.y_offset,
                        w: neck.width,
                        h: neck.height
                    })))(Data_Foldable.traverse_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)(paint_fret(tuple))(Data_Array.range(0)(neck.num_frets))))(paint_inlays(tuple)))(paint_strings(tuple)))(Data_Foldable.traverse_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)(paint_number(tuple))(Data_Array.range(1)(neck.num_frets)))();
                };
            };
            throw new Error("Failed pattern match at UI.GuitarNeckCanvas line 40, column 3 - line 41, column 3: " + [ v.constructor.name ]);
        })())();
    });
};
var chord_color = "#ee1122";
var paint_centeroid = function (ctx) {
    return function (fing) {
        return function __do() {
            var v = Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Graphics_Canvas.createRadialGradient({
                x0: fing.centeroid.x,
                y0: fing.centeroid.y,
                r0: 0.0,
                x1: fing.centeroid.x,
                y1: fing.centeroid.y,
                r1: 40.0
            })(ctx))(Graphics_Canvas.addColorStop(0.0)(chord_color)))(Graphics_Canvas.addColorStop(1.0)(transparent))();
            return Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Graphics_Canvas.beginPath(ctx))(Graphics_Canvas.setGradientFillStyle(v)))(Data_Function.flip(Graphics_Canvas.arc)(circle({
                x: fing.centeroid.x,
                y: fing.centeroid.y,
                r: 40.0
            }))))(Graphics_Canvas.fill))();
        };
    };
};
var paint_chord = function (v) {
    return function (fing) {
        var paint_fingering = function (ctx1) {
            return function (v1) {
                return Data_Functor["void"](Control_Monad_Eff.functorEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Control_Bind.bind(Control_Monad_Eff.bindEff)(Graphics_Canvas.beginPath(ctx1))(Graphics_Canvas.setFillStyle(chord_color)))(Data_Function.flip(Graphics_Canvas.arc)(circle({
                    x: v1.x,
                    y: v1.y,
                    r: 8.5
                }))))(Graphics_Canvas.fill));
            };
        };
        return Data_Foldable.traverse_(Control_Monad_Eff.applicativeEff)(Data_Foldable.foldableArray)(paint_fingering(v.value0))(Fingering.to_points(v.value1)(fing.fingering));
    };
};
var display_neck = function (state) {
    var f = function (ctx) {
        return function (fing) {
            return function (n) {
                var $70 = Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqInt))(state.focused)(new Data_Maybe.Just(n));
                if ($70) {
                    return paint_chord(new Data_Tuple.Tuple(ctx, state.neck_data))(fing);
                };
                return paint_centeroid(ctx)(fing);
            };
        };
    };
    return Data_Functor["void"](Control_Monad_Eff.functorEff)(function __do() {
        var v = Graphics_Canvas.getCanvasElementById("guitar_notes")();
        var __unused = function (dictPartial1) {
            return function ($dollar27) {
                return $dollar27;
            };
        };
        return __unused()((function () {
            if (v instanceof Data_Maybe.Just) {
                return function __do() {
                    var v1 = Graphics_Canvas.getContext2D(v.value0)();
                    return Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Control_Apply.applySecond(Control_Monad_Eff.applyEff)(Graphics_Canvas.setGlobalAlpha(v1)(0.5))(Graphics_Canvas.setFillStyle(chord_color)(v1)))(Data_Traversable.sequence(Data_Traversable.traversableArray)(Control_Monad_Eff.applicativeEff)(Data_Array.zipWith(f(v1))(state.curr_fingerings)(Data_Array.range(0)(Data_Array.length(state.curr_fingerings) - 1 | 0)))))(Graphics_Canvas.setGlobalAlpha(v1)(1.0))();
                };
            };
            throw new Error("Failed pattern match at UI.GuitarNeckCanvas line 142, column 3 - line 143, column 3: " + [ v.constructor.name ]);
        })())();
    });
};
module.exports = {
    paint_neck: paint_neck,
    display_neck: display_neck,
    wipe_neck: wipe_neck
};
