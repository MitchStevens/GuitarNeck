"use strict";
var Data_Semiring = require("../Data.Semiring");
var Graphics_Canvas = require("../Graphics.Canvas");
var $$Math = require("../Math");
var Prelude = require("../Prelude");
var circle = function (c) {
    return {
        x: c.x,
        y: c.y,
        r: c.r,
        start: 0.0,
        end: 2.0 * $$Math.pi
    };
};
module.exports = {
    circle: circle
};
