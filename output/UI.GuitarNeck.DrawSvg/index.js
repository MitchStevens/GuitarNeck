"use strict";
var CSS = require("../CSS");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Functor = require("../Data.Functor");
var Data_Int = require("../Data.Int");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Tuple = require("../Data.Tuple");
var Halogen = require("../Halogen");
var Halogen_HTML = require("../Halogen.HTML");
var Halogen_HTML_Core = require("../Halogen.HTML.Core");
var Halogen_HTML_Properties = require("../Halogen.HTML.Properties");
var $$Math = require("../Math");
var NeckData = require("../NeckData");
var Point = require("../Point");
var Prelude = require("../Prelude");
var Svg_Attributes = require("../Svg.Attributes");
var Svg_Elements = require("../Svg.Elements");
var scale_length = 32.4;
var num_frets = 15;

//Fender Strat. neck data
var fret_width = 0.2413;
var fingerboard_length = 2.0 * scale_length * (1.0 - $$Math.pow(0.5)(Data_Int.toNumber(num_frets) / 12.0));
var draw_neck = function (neck) {
    var draw_strings = (function () {
        var gauges = [ 0.254, 0.3302, 0.4318, 0.6604, 0.9144, 1.1684 ];
        var single_string = function (n) {
            return function (gauge) {
                return Svg_Elements.rect([ Svg_Attributes.x(0.0), Svg_Attributes.y((2.0 * n + 1.0) / 12.0 - gauge / 50.0 / 2.0), Svg_Attributes.height(gauge / 50.0), Svg_Attributes.width(1.0) ]);
            };
        };
        return Svg_Elements.g([ Halogen_HTML_Properties.id_("strings"), Svg_Attributes.transform([ new Svg_Attributes.Translate(neck.x_offset, neck.y_offset), new Svg_Attributes.Scale(neck.width - neck.x_offset, neck.height - neck.y_offset) ]) ])(Data_Array.zipWith(function ($4) {
            return single_string(Data_Int.toNumber($4));
        })(Data_Array.range(0)(5))(gauges));
    })();
    var draw_inlays = (function () {
        var single_inlay = function (v) {
            return Svg_Elements.circle([ Halogen_HTML_Properties.class_("inlay"), Svg_Attributes.cx(v.value0), Svg_Attributes.cy(v.value1) ]);
        };
        var inlay_points = Data_Semigroup.append(Data_Semigroup.semigroupArray)([ new Data_Tuple.Tuple(12.0, 0.5), new Data_Tuple.Tuple(12.0, 4.5) ])(Data_Functor.map(Data_Functor.functorArray)(function ($5) {
            return (function (x) {
                return new Data_Tuple.Tuple(x, 2.5);
            })(Data_Int.toNumber($5));
        })([ 3, 5, 7, 9, 15, 17, 19, 21 ]));
        return Svg_Elements.g([ Halogen_HTML_Properties.id_("inlays"), Svg_Attributes.transform([ new Svg_Attributes.Translate(neck.x_offset, neck.y_offset), new Svg_Attributes.Scale(1.0, 1.0) ]) ])(Data_Functor.map(Data_Functor.functorArray)(single_inlay)(inlay_points));
    })();
    var draw_frets = (function () {
        var f = function (x) {
            var k = $$Math.log(0.5) / 12.0;
            return draw_inlays;
        };
        var draw_fret = function (fret) {
            return Svg_Elements.rect([ Svg_Attributes.x(1.0 - $$Math.pow(2.0)(-Data_Int.toNumber(fret) / 12.0)), Svg_Attributes.y(0.0), Svg_Attributes.height(1.0), Svg_Attributes.width(4.0e-3) ]);
        };
        return Svg_Elements.g([ Svg_Attributes.transform([ new Svg_Attributes.Translate(neck.x_offset, neck.y_offset), new Svg_Attributes.Scale(fingerboard_length, neck.height - neck.y_offset) ]), Halogen_HTML_Properties.id_("frets") ])(Data_Functor.map(Data_Functor.functorArray)(draw_fret)(Data_Array.range(1)(num_frets)));
    })();
    var draw_fretboard = Svg_Elements.rect([ Svg_Attributes.x(neck.x_offset), Svg_Attributes.y(neck.y_offset), Svg_Attributes.height(neck.height - neck.x_offset), Svg_Attributes.width(neck.width - neck.y_offset), Halogen_HTML_Properties.id_("fretboard") ]);
    return Svg_Elements.svg([ Svg_Attributes.viewBox(0.0)(0.0)(neck.width)(neck.height) ])([ draw_fretboard, draw_frets ]);
};
module.exports = {
    num_frets: num_frets,
    fret_width: fret_width,
    scale_length: scale_length,
    fingerboard_length: fingerboard_length,
    draw_neck: draw_neck
};
