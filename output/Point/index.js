"use strict";
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Int = require("../Data.Int");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var $$Math = require("../Math");
var Prelude = require("../Prelude");
var Point = function (x) {
    return x;
};
var show_point = new Data_Show.Show(function (v) {
    return "(" + (Data_Show.show(Data_Show.showNumber)(v.x) + (", " + (Data_Show.show(Data_Show.showNumber)(v.y) + ")")));
});
var semiring_point = new Data_Semiring.Semiring(function (v) {
    return function (v1) {
        return {
            x: v.x + v1.x,
            y: v.y + v1.y
        };
    };
}, function (v) {
    return function (v1) {
        return {
            x: v.x * v1.x,
            y: v.y * v1.y
        };
    };
}, {
    x: 1.0,
    y: 1.0
}, {
    x: 0.0,
    y: 0.0
});
var ring_point = new Data_Ring.Ring(function () {
    return semiring_point;
}, function (v) {
    return function (v1) {
        return {
            x: v.x - v1.x,
            y: v.y - v1.y
        };
    };
});
var point = function (x) {
    return function (y) {
        return {
            x: x,
            y: y
        };
    };
};
var point_int = function (x) {
    return function (y) {
        return point(Data_Int.toNumber(x))(Data_Int.toNumber(y));
    };
};
var factor = function (n) {
    return function (v) {
        return {
            x: n * v.x,
            y: n * v.y
        };
    };
};
var eq_point = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x.x === y.x && x.y === y.y;
    };
});
var ord_point = new Data_Ord.Ord(function () {
    return eq_point;
}, function (x) {
    return function (y) {
        var v = Data_Ord.compare(Data_Ord.ordNumber)(x.x)(y.x);
        if (v instanceof Data_Ordering.LT) {
            return Data_Ordering.LT.value;
        };
        if (v instanceof Data_Ordering.GT) {
            return Data_Ordering.GT.value;
        };
        return Data_Ord.compare(Data_Ord.ordNumber)(x.y)(y.y);
    };
});
var distance = function (p1) {
    return function (p2) {
        var v = Data_Ring.sub(ring_point)(p1)(p2);
        return $$Math.sqrt($$Math.pow(v.x)(2.0) + $$Math.pow(v.y)(2.0));
    };
};
module.exports = {
    Point: Point,
    factor: factor,
    distance: distance,
    point: point,
    point_int: point_int,
    eq_point: eq_point,
    ord_point: ord_point,
    semiring_point: semiring_point,
    ring_point: ring_point,
    show_point: show_point
};
