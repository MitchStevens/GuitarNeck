"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Category = require("../Control.Category");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DOM_Event_Types = require("../DOM.Event.Types");
var Data_Argonaut_Decode = require("../Data.Argonaut.Decode");
var Data_Array = require("../Data.Array");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Int = require("../Data.Int");
var Data_Lattice = require("../Data.Lattice");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Debug_Trace = require("../Debug.Trace");
var Fret = require("../Fret");
var Interval = require("../Interval");
var $$Math = require("../Math");
var Music_Transpose = require("../Music.Transpose");
var NeckData = require("../NeckData");
var Point = require("../Point");
var Prelude = require("../Prelude");
var Fingering = function (x) {
    return x;
};
var to_array = function (v) {
    return [ v.e4, v.b3, v.g3, v.d3, v.a2, v.e2 ];
};
var to_points = function (neck_data) {
    return function (chord) {
        var xs = Data_Functor.map(Data_Functor.functorArray)(Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
            return v;
        }))(to_array(chord));
        var maybe_point = function (mx) {
            return function (y) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Function.flip(Point.point_int)(y))(mx);
            };
        };
        var f = NeckData.fret_transformation(neck_data);
        return Data_Functor.map(Data_Functor.functorArray)(f)(Data_Array.catMaybes(Data_Array.zipWith(maybe_point)(xs)(Data_Array.range(0)(5))));
    };
};
var get_closest = function (dictFoldable) {
    return function (neck_data) {
        return function (point) {
            return function (chords) {
                return Data_Foldable.minimumBy(dictFoldable)(Data_Ord.comparing(Data_Ord.ordNumber)(function (cd) {
                    return Point.distance(point)(cd.centeroid);
                }))(chords);
            };
        };
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
var fret_interval = function (fingering) {
    var frets = Data_Functor.map(Data_Functor.functorArray)(Interval.singleton(Fret.ord_fret))(Data_Array.catMaybes(to_array(fingering)));
    return Data_Foldable.foldl(Data_Foldable.foldableArray)(Data_Lattice.join(Interval.join_interval(Fret.ord_fret)))(Data_Lattice.bottom(Interval.boundedjoin_interval(Fret.ord_fret)))(frets);
};
var shift_fingering = function (n) {
    return function (fingering) {
        var new_fingering = Data_Functor.map(functor_chord)(Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
            return Data_Semiring.add(Fret.semiring_fret)(v)(n);
        }))(fingering);
        var range = fret_interval(new_fingering);
        var a = Debug_Trace.trace(Debug_Trace.warn())(Data_Show.show(Interval.show_interval(Fret.show_fret))(range))(Control_Category.id(Control_Category.categoryFn));
        var $16 = Data_Eq.eq(Interval.eq_interval(Fret.eq_fret))(Data_Lattice.meet(Interval.meet_interval(Fret.ord_fret))(range)(Interval.interval(Fret.ord_fret)(0)(11)))(Interval.EmptyInterval.value);
        if ($16) {
            return Data_Functor.map(functor_chord)(Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
                return Data_Ring.sub(Fret.ring_fret)(v)(12);
            }))(new_fingering);
        };
        return new_fingering;
    };
};
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
var show_fingering = new Data_Show.Show(function (chord) {
    return Data_Foldable.intercalate(foldable_chord)(Data_Monoid.monoidString)("-")(Data_Functor.map(functor_chord)(Data_Maybe.maybe("x")(Data_Show.show(Fret.show_fret)))(chord));
});
var closest_index = function (neck) {
    return function (point) {
        return function (fings) {
            var dist = function (fd) {
                return Point.distance(point)(fd.centeroid);
            };
            var f = function (i) {
                return function (j) {
                    return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(dist)(Data_Array.index(fings)(i)))(function (v) {
                        return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(dist)(Data_Array.index(fings)(j)))(function (v1) {
                            return Control_Applicative.pure(Data_Maybe.applicativeMaybe)((function () {
                                var $19 = v < v1;
                                if ($19) {
                                    return i;
                                };
                                return j;
                            })());
                        });
                    });
                };
            };
            return Data_Array.foldM(Data_Maybe.monadMaybe)(f)(0)(Data_Array.range(0)(Data_Foldable.length(Data_Foldable.foldableArray)(Data_Semiring.semiringInt)(fings) - 1 | 0));
        };
    };
};
var centeroid_chord = function (neck_data) {
    return function (chord) {
        var points = to_points(neck_data)(chord);
        var $20 = Data_Foldable["null"](Data_Foldable.foldableArray)(points);
        if ($20) {
            return Data_Maybe.Nothing.value;
        };
        return Data_Maybe.Just.create(Point.factor(1.0 / Data_Int.toNumber(Data_Foldable.length(Data_Foldable.foldableArray)(Data_Semiring.semiringInt)(points)))(Data_Foldable.sum(Data_Foldable.foldableArray)(Point.semiring_point)(points)));
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
    Fingering: Fingering,
    shift_fingering: shift_fingering,
    to_array: to_array,
    fret_interval: fret_interval,
    cache_centeroid: cache_centeroid,
    to_points: to_points,
    centeroid_chord: centeroid_chord,
    closest_index: closest_index,
    get_closest: get_closest,
    functor_chord: functor_chord,
    apply_chord: apply_chord,
    applicative_chord: applicative_chord,
    foldable_chord: foldable_chord,
    show_fingering: show_fingering
};
