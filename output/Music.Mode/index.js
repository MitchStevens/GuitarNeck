"use strict";
var Data_Array = require("../Data.Array");
var Data_Bounded = require("../Data.Bounded");
var Data_Enum = require("../Data.Enum");
var Data_Eq = require("../Data.Eq");
var Data_Generic_Rep = require("../Data.Generic.Rep");
var Data_Generic_Rep_Bounded = require("../Data.Generic.Rep.Bounded");
var Data_Generic_Rep_Enum = require("../Data.Generic.Rep.Enum");
var Data_Generic_Rep_Show = require("../Data.Generic.Rep.Show");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Music_Degree = require("../Music.Degree");
var Prelude = require("../Prelude");
var Major = (function () {
    function Major() {

    };
    Major.value = new Major();
    return Major;
})();
var Minor = (function () {
    function Minor() {

    };
    Minor.value = new Minor();
    return Minor;
})();
var Ionian = (function () {
    function Ionian() {

    };
    Ionian.value = new Ionian();
    return Ionian;
})();
var Dorian = (function () {
    function Dorian() {

    };
    Dorian.value = new Dorian();
    return Dorian;
})();
var Phrygian = (function () {
    function Phrygian() {

    };
    Phrygian.value = new Phrygian();
    return Phrygian;
})();
var Lydian = (function () {
    function Lydian() {

    };
    Lydian.value = new Lydian();
    return Lydian;
})();
var Mixolydian = (function () {
    function Mixolydian() {

    };
    Mixolydian.value = new Mixolydian();
    return Mixolydian;
})();
var Aeolian = (function () {
    function Aeolian() {

    };
    Aeolian.value = new Aeolian();
    return Aeolian;
})();
var Locrian = (function () {
    function Locrian() {

    };
    Locrian.value = new Locrian();
    return Locrian;
})();
var Augmented = (function () {
    function Augmented() {

    };
    Augmented.value = new Augmented();
    return Augmented;
})();
var Diminished = (function () {
    function Diminished() {

    };
    Diminished.value = new Diminished();
    return Diminished;
})();
var show_mode = new Data_Show.Show(function (v) {
    if (v instanceof Major) {
        return "maj";
    };
    if (v instanceof Minor) {
        return "min";
    };
    if (v instanceof Ionian) {
        return "min";
    };
    if (v instanceof Dorian) {
        return "min";
    };
    if (v instanceof Phrygian) {
        return "min";
    };
    if (v instanceof Lydian) {
        return "maj";
    };
    if (v instanceof Mixolydian) {
        return "dom";
    };
    if (v instanceof Aeolian) {
        return "min";
    };
    if (v instanceof Locrian) {
        return "min";
    };
    if (v instanceof Augmented) {
        return "aug";
    };
    if (v instanceof Diminished) {
        return "dim";
    };
    throw new Error("Failed pattern match at Music.Mode line 31, column 10 - line 43, column 1: " + [ v.constructor.name ]);
});

/**
 *  Get a mode of the major scale, 1 is major, 2 is Dorian, 3 is Phyrigian, etc. starting from a root pitch 
 */
var scale_mode = function (degree) {
    var rotate = function (n) {
        return function (list) {
            return Data_Semigroup.append(Data_Semigroup.semigroupArray)(Data_Array.drop(n)(list))(Data_Array.take(n)(list));
        };
    };
    var major_intervals = [ 2, 2, 1, 2, 2, 2, 1 ];
    return rotate(Data_Enum.fromEnum(Music_Degree.boundedenum_degree)(degree))(major_intervals);
};
var num_notes = function (mode) {
    if (mode instanceof Diminished) {
        return 8;
    };
    if (mode instanceof Augmented) {
        return 6;
    };
    return 7;
};
var intervals = function (v) {
    if (v instanceof Major) {
        return scale_mode(Music_Degree.I.value);
    };
    if (v instanceof Minor) {
        return scale_mode(Music_Degree.VI.value);
    };
    if (v instanceof Ionian) {
        return scale_mode(Music_Degree.I.value);
    };
    if (v instanceof Dorian) {
        return scale_mode(Music_Degree.II.value);
    };
    if (v instanceof Phrygian) {
        return scale_mode(Music_Degree.III.value);
    };
    if (v instanceof Lydian) {
        return scale_mode(Music_Degree.IV.value);
    };
    if (v instanceof Mixolydian) {
        return scale_mode(Music_Degree.V.value);
    };
    if (v instanceof Aeolian) {
        return scale_mode(Music_Degree.VI.value);
    };
    if (v instanceof Locrian) {
        return scale_mode(Music_Degree.VII.value);
    };
    if (v instanceof Diminished) {
        return [ 2, 1, 2, 1, 2, 1, 2, 1 ];
    };
    if (v instanceof Augmented) {
        return [ 3, 1, 3, 1, 3, 1 ];
    };
    throw new Error("Failed pattern match at Music.Mode line 55, column 13 - line 66, column 35: " + [ v.constructor.name ]);
};
var generic_mode = new Data_Generic_Rep.Generic(function (x) {
    if (x instanceof Major) {
        return new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value);
    };
    if (x instanceof Minor) {
        return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value));
    };
    if (x instanceof Ionian) {
        return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value)));
    };
    if (x instanceof Dorian) {
        return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value))));
    };
    if (x instanceof Phrygian) {
        return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value)))));
    };
    if (x instanceof Lydian) {
        return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value))))));
    };
    if (x instanceof Mixolydian) {
        return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value)))))));
    };
    if (x instanceof Aeolian) {
        return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value))))))));
    };
    if (x instanceof Locrian) {
        return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value)))))))));
    };
    if (x instanceof Augmented) {
        return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value))))))))));
    };
    if (x instanceof Diminished) {
        return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(Data_Generic_Rep.NoArguments.value))))))))));
    };
    throw new Error("Failed pattern match at Music.Mode line 27, column 8 - line 27, column 48: " + [ x.constructor.name ]);
}, function (x) {
    if (x instanceof Data_Generic_Rep.Inl) {
        return Major.value;
    };
    if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inl) {
        return Minor.value;
    };
    if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0 instanceof Data_Generic_Rep.Inl)) {
        return Ionian.value;
    };
    if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0 instanceof Data_Generic_Rep.Inl))) {
        return Dorian.value;
    };
    if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl)))) {
        return Phrygian.value;
    };
    if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl))))) {
        return Lydian.value;
    };
    if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl)))))) {
        return Mixolydian.value;
    };
    if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl))))))) {
        return Aeolian.value;
    };
    if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl)))))))) {
        return Locrian.value;
    };
    if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inl))))))))) {
        return Augmented.value;
    };
    if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0.value0.value0.value0.value0.value0.value0 instanceof Data_Generic_Rep.Inr))))))))) {
        return Diminished.value;
    };
    throw new Error("Failed pattern match at Music.Mode line 27, column 8 - line 27, column 48: " + [ x.constructor.name ]);
});
var eq_mode = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof Major && y instanceof Major) {
            return true;
        };
        if (x instanceof Minor && y instanceof Minor) {
            return true;
        };
        if (x instanceof Ionian && y instanceof Ionian) {
            return true;
        };
        if (x instanceof Dorian && y instanceof Dorian) {
            return true;
        };
        if (x instanceof Phrygian && y instanceof Phrygian) {
            return true;
        };
        if (x instanceof Lydian && y instanceof Lydian) {
            return true;
        };
        if (x instanceof Mixolydian && y instanceof Mixolydian) {
            return true;
        };
        if (x instanceof Aeolian && y instanceof Aeolian) {
            return true;
        };
        if (x instanceof Locrian && y instanceof Locrian) {
            return true;
        };
        if (x instanceof Augmented && y instanceof Augmented) {
            return true;
        };
        if (x instanceof Diminished && y instanceof Diminished) {
            return true;
        };
        return false;
    };
});
var ord_mode = new Data_Ord.Ord(function () {
    return eq_mode;
}, function (x) {
    return function (y) {
        if (x instanceof Major && y instanceof Major) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Major) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Major) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Minor && y instanceof Minor) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Minor) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Minor) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Ionian && y instanceof Ionian) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Ionian) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Ionian) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Dorian && y instanceof Dorian) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Dorian) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Dorian) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Phrygian && y instanceof Phrygian) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Phrygian) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Phrygian) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Lydian && y instanceof Lydian) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Lydian) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Lydian) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Mixolydian && y instanceof Mixolydian) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Mixolydian) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Mixolydian) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Aeolian && y instanceof Aeolian) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Aeolian) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Aeolian) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Locrian && y instanceof Locrian) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Locrian) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Locrian) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Augmented && y instanceof Augmented) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Augmented) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Augmented) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Diminished && y instanceof Diminished) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at Music.Mode line 29, column 8 - line 29, column 42: " + [ x.constructor.name, y.constructor.name ]);
    };
});
var enum_pitch = new Data_Enum.Enum(function () {
    return ord_mode;
}, Data_Generic_Rep_Enum.genericPred(generic_mode)(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments)))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments))))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments))))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments))))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments))))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments))))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments))))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments))))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments))))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments)))), Data_Generic_Rep_Enum.genericSucc(generic_mode)(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumSum(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericTopConstructor(Data_Generic_Rep_Bounded.genericTopNoArguments))(Data_Generic_Rep_Enum.genericEnumConstructor(Data_Generic_Rep_Enum.genericEnumNoArguments))(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments)))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments))))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments))))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments))))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments))))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments))))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments))))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments))))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments))))(Data_Generic_Rep_Bounded.genericBottomSum(Data_Generic_Rep_Bounded.genericBottomConstructor(Data_Generic_Rep_Bounded.genericBottomNoArguments)))));
var bounded_pitch = new Data_Bounded.Bounded(function () {
    return ord_mode;
}, Major.value, Diminished.value);
var boundedenum_mode = new Data_Enum.BoundedEnum(function () {
    return bounded_pitch;
}, function () {
    return enum_pitch;
}, 11, Data_Enum.defaultFromEnum(enum_pitch), Data_Enum.defaultToEnum(bounded_pitch)(enum_pitch));
module.exports = {
    Major: Major,
    Minor: Minor,
    Ionian: Ionian,
    Dorian: Dorian,
    Phrygian: Phrygian,
    Lydian: Lydian,
    Mixolydian: Mixolydian,
    Aeolian: Aeolian,
    Locrian: Locrian,
    Augmented: Augmented,
    Diminished: Diminished,
    num_notes: num_notes,
    intervals: intervals,
    generic_mode: generic_mode,
    eq_mode: eq_mode,
    ord_mode: ord_mode,
    show_mode: show_mode,
    enum_pitch: enum_pitch,
    bounded_pitch: bounded_pitch,
    boundedenum_mode: boundedenum_mode
};