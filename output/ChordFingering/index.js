"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DOM_Event_Types = require("../DOM.Event.Types");
var Data_Array = require("../Data.Array");
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
var Fret = require("../Fret");
var $$Math = require("../Math");
var NeckData = require("../NeckData");
var Prelude = require("../Prelude");
var Point = function (x) {
    return x;
};

//--------------------------------
var Fingering = function (x) {
    return x;
};
var to_array = function (v) {
    return [ v.e4, v.b3, v.g3, v.d3, v.a2, v.e2 ];
};
var show_point = new Data_Show.Show(function (v) {
    return "(" + (Data_Show.show(Data_Show.showNumber)(v.x) + (", " + (Data_Show.show(Data_Show.showNumber)(v.y) + ")")));
});
var show_chord = new Data_Show.Show(function (chord) {
    var f = Data_Maybe.maybe("x")(Data_Show.show(Fret.show_fret));
    return Data_Foldable.foldl(Data_Foldable.foldableArray)(function (b) {
        return function (a) {
            return b + ("-" + f(a));
        };
    })("")(to_array(chord));
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
    y: 0.0
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
var to_points = function (neck) {
    return function (chord) {
        var ys = Data_Functor.map(Data_Functor.functorArray)(NeckData.str_y(neck))([ 0.0, 1.0, 2.0, 3.0, 4.0, 5.0 ]);
        var xs = Data_Functor.map(Data_Functor.functorArray)(Data_Functor.map(Data_Maybe.functorMaybe)(NeckData.fret_marker(neck)))(to_array(chord));
        var maybe_point = function (mx) {
            return function (y) {
                return Control_Bind.bind(Data_Maybe.bindMaybe)(mx)(function (v) {
                    return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(point(v)(y));
                });
            };
        };
        return Data_Array.catMaybes(Data_Array.zipWith(maybe_point)(xs)(ys));
    };
};
var functor_chord = new Data_Functor.Functor(function (f) {
    return function (v) {
        return {
            e4: f(v.e4),
            b3: f(v.b3),
            g3: f(v.g3),
            d3: f(v.d3),
            a2: f(v.a2),
            e2: f(v.e2)
        };
    };
});
var foldable_chord = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (chord) {
            return Data_Foldable.foldMap(Data_Foldable.foldableArray)(dictMonoid)(f)(to_array(chord));
        };
    };
}, function (f) {
    return function (z) {
        return function (chord) {
            return Data_Foldable.foldl(Data_Foldable.foldableArray)(f)(z)(to_array(chord));
        };
    };
}, function (f) {
    return function (z) {
        return function (chord) {
            return Data_Foldable.foldr(Data_Foldable.foldableArray)(f)(z)(to_array(chord));
        };
    };
});
var factor = function (n) {
    return function (v) {
        return {
            x: n * v.x,
            y: n * v.y
        };
    };
};
var distance = function (p1) {
    return function (p2) {
        var v = Data_Ring.sub(ring_point)(p1)(p2);
        return $$Math.sqrt($$Math.pow(v.x)(2.0) + $$Math.pow(v.y)(2.0));
    };
};
var get_closest = function (dictFoldable) {
    return function (neck_data) {
        return function (point1) {
            return function (chords) {
                return Data_Foldable.minimumBy(dictFoldable)(Data_Ord.comparing(Data_Ord.ordNumber)(function (cd) {
                    return distance(point1)(cd.centeroid);
                }))(chords);
            };
        };
    };
};
var centeroid_chord = function (neck_data) {
    return function (chord) {
        var points = to_points(neck_data)(chord);
        var $31 = Data_Foldable["null"](Data_Foldable.foldableArray)(points);
        if ($31) {
            return Data_Maybe.Nothing.value;
        };
        return Data_Maybe.Just.create(factor(1.0 / Data_Int.toNumber(Data_Foldable.length(Data_Foldable.foldableArray)(Data_Semiring.semiringInt)(points)))(Data_Foldable.sum(Data_Foldable.foldableArray)(semiring_point)(points)));
    };
};
var cache_centeroid = function (neck) {
    return function (chord) {
        return Control_Bind.bind(Data_Maybe.bindMaybe)(centeroid_chord(neck)(chord))(function (v) {
            return Control_Applicative.pure(Data_Maybe.applicativeMaybe)({
                fingering: chord,
                centeroid: v
            });
        });
    };
};
var apply_chord = new Control_Apply.Apply(function () {
    return functor_chord;
}, function (v) {
    return function (v1) {
        return {
            e4: v.e4(v1.e4),
            b3: v.b3(v1.b3),
            g3: v.g3(v1.g3),
            d3: v.d3(v1.d3),
            a2: v.a2(v1.a2),
            e2: v.e2(v1.e2)
        };
    };
});
var applicative_chord = new Control_Applicative.Applicative(function () {
    return apply_chord;
}, function (c) {
    return {
        e4: c,
        b3: c,
        g3: c,
        d3: c,
        a2: c,
        e2: c
    };
});
module.exports = {
    Point: Point,
    factor: factor,
    distance: distance,
    point: point,
    Fingering: Fingering,
    to_array: to_array,
    cache_centeroid: cache_centeroid,
    to_points: to_points,
    centeroid_chord: centeroid_chord,
    get_closest: get_closest,
    semiring_point: semiring_point,
    ring_point: ring_point,
    show_point: show_point,
    functor_chord: functor_chord,
    apply_chord: apply_chord,
    applicative_chord: applicative_chord,
    foldable_chord: foldable_chord,
    show_chord: show_chord
};
