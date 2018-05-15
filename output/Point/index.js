"use strict";
var Control_Biapplicative = require("../Control.Biapplicative");
var Control_Biapply = require("../Control.Biapply");
var Data_Bifunctor = require("../Data.Bifunctor");
var Data_Function = require("../Data.Function");
var Data_Int = require("../Data.Int");
var Data_Lens = require("../Data.Lens");
var Data_Lens_Lens_Tuple = require("../Data.Lens.Lens.Tuple");
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var Data_Tuple = require("../Data.Tuple");
var $$Math = require("../Math");
var Prelude = require("../Prelude");
var point_int = function (x) {
    return function (y) {
        return Data_Bifunctor.bimap(Data_Tuple.bifunctorTuple)(Data_Int.toNumber)(Data_Int.toNumber)(new Data_Tuple.Tuple(x, y));
    };
};
var factor = function (n) {
    return Data_Bifunctor.bimap(Data_Tuple.bifunctorTuple)(function (v) {
        return n * v;
    })(function (v) {
        return n * v;
    });
};
var distance = function (p1) {
    return function (p2) {
        var v = Data_Ring.sub(Data_Tuple.ringTuple(Data_Ring.ringNumber)(Data_Ring.ringNumber))(p1)(p2);
        return $$Math.sqrt(v.value0 * v.value0 + v.value1 * v.value1);
    };
};
var _y = function (dictStrong) {
    return Data_Lens_Lens_Tuple._2(dictStrong);
};
var _x = function (dictStrong) {
    return Data_Lens_Lens_Tuple._1(dictStrong);
};
module.exports = {
    _x: _x,
    _y: _y,
    factor: factor,
    distance: distance,
    point_int: point_int
};
