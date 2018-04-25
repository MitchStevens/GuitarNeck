// Generated by purs version 0.11.7
"use strict";
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Functor = require("../Data.Functor");
var Data_Lazy = require("../Data.Lazy");
var Data_Monoid = require("../Data.Monoid");
var Data_Monoid_Additive = require("../Data.Monoid.Additive");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_Traversable = require("../Data.Traversable");
var Prelude = require("../Prelude");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var NoKey = (function () {
    function NoKey() {

    };
    NoKey.value = new NoKey();
    return NoKey;
})();
var Key = (function () {
    function Key(value0) {
        this.value0 = value0;
    };
    Key.create = function (value0) {
        return new Key(value0);
    };
    return Key;
})();
var Elem = function (x) {
    return x;
};
var Measured = function (measure) {
    this.measure = measure;
};
var strJoin = function (dictShow) {
    return function (glue) {
        return function ($72) {
            return Data_Foldable.intercalate(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(glue)(Data_Functor.map(Data_Functor.functorArray)(Data_Show.show(dictShow))($72));
        };
    };
};
var showKey = function (dictShow) {
    return new Data_Show.Show(function (v) {
        if (v instanceof Key) {
            return "(Key " + (Data_Show.show(dictShow)(v.value0) + ")");
        };
        if (v instanceof NoKey) {
            return "NoKey";
        };
        throw new Error("Failed pattern match at Data.Sequence.Internal line 107, column 1 - line 107, column 45: " + [ v.constructor.name ]);
    });
};
var semigroupKey = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        if (v1 instanceof NoKey) {
            return v;
        };
        return v1;
    };
});
var monoidKey = new Data_Monoid.Monoid(function () {
    return semigroupKey;
}, NoKey.value);
var measuredElemKey = new Measured(function (v) {
    return new Key(v);
});
var measuredElem = new Measured(function (v) {
    return 1;
});
var measure = function (dict) {
    return dict.measure;
};
var measuredArray = function (dictMonoid) {
    return function (dictMeasured) {
        return new Measured(function (xs) {
            return Data_Foldable.foldl(Data_Foldable.foldableArray)(function (i) {
                return function (a) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(i)(measure(dictMeasured)(a));
                };
            })(Data_Monoid.mempty(dictMonoid))(xs);
        });
    };
};
var measuredLazy = function (dictMonoid) {
    return function (dictMeasured) {
        return new Measured(function (s) {
            return measure(dictMeasured)(Data_Lazy.force(s));
        });
    };
};
var mapmap = function (dictFunctor) {
    return function (dictFunctor1) {
        return function ($73) {
            return Data_Functor.map(dictFunctor)(Data_Functor.map(dictFunctor1)($73));
        };
    };
};
var mapmapmap = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (dictFunctor2) {
            return function ($74) {
                return mapmap(dictFunctor)(dictFunctor1)(Data_Functor.map(dictFunctor2)($74));
            };
        };
    };
};
var mapGetElem = function (dictFunctor) {
    return Unsafe_Coerce.unsafeCoerce;
};
var mapElem = function (dictFunctor) {
    return Unsafe_Coerce.unsafeCoerce;
};
var liftElem = Unsafe_Coerce.unsafeCoerce;
var lift2Elem = Unsafe_Coerce.unsafeCoerce;
var getElem = function (v) {
    return v;
};
var showElem = function (dictShow) {
    return new Data_Show.Show(function (x) {
        return "Elem (" + (Data_Show.show(dictShow)(getElem(x)) + ")");
    });
};
var functorElem = new Data_Functor.Functor(function (f) {
    return function (v) {
        return f(v);
    };
});
var foldableElem = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function (v) {
            return f(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(z)(v);
        };
    };
}, function (f) {
    return function (z) {
        return function (v) {
            return f(v)(z);
        };
    };
});
var traversableElem = new Data_Traversable.Traversable(function () {
    return foldableElem;
}, function () {
    return functorElem;
}, function (dictApplicative) {
    return function (v) {
        return mapElem((dictApplicative.Apply0()).Functor0())(v);
    };
}, function (dictApplicative) {
    return function (f) {
        return function (v) {
            return mapElem((dictApplicative.Apply0()).Functor0())(f(v));
        };
    };
});
var eqKey = function (dictEq) {
    return new Data_Eq.Eq(function (v) {
        return function (v1) {
            if (v instanceof Key && v1 instanceof Key) {
                return Data_Eq.eq(dictEq)(v.value0)(v1.value0);
            };
            if (v instanceof NoKey && v1 instanceof NoKey) {
                return true;
            };
            return false;
        };
    });
};
var ordKey = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqKey(dictOrd.Eq0());
    }, function (v) {
        return function (v1) {
            if (v instanceof NoKey) {
                return Data_Ordering.LT.value;
            };
            if (v1 instanceof NoKey) {
                return Data_Ordering.GT.value;
            };
            if (v instanceof Key && v1 instanceof Key) {
                return Data_Ord.compare(dictOrd)(v.value0)(v1.value0);
            };
            throw new Error("Failed pattern match at Data.Sequence.Internal line 115, column 1 - line 115, column 42: " + [ v.constructor.name, v1.constructor.name ]);
        };
    });
};
var eqElem = function (dictEq) {
    return new Data_Eq.Eq(function (v) {
        return function (v1) {
            return Data_Eq.eq(dictEq)(v)(v1);
        };
    });
};
var ordElem = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqElem(dictOrd.Eq0());
    }, function (v) {
        return function (v1) {
            return Data_Ord.compare(dictOrd)(v)(v1);
        };
    });
};
module.exports = {
    mapmap: mapmap,
    mapmapmap: mapmapmap,
    strJoin: strJoin,
    Measured: Measured,
    measure: measure,
    Elem: Elem,
    getElem: getElem,
    mapElem: mapElem,
    mapGetElem: mapGetElem,
    lift2Elem: lift2Elem,
    liftElem: liftElem,
    NoKey: NoKey,
    Key: Key,
    measuredArray: measuredArray,
    measuredLazy: measuredLazy,
    measuredElem: measuredElem,
    showElem: showElem,
    eqElem: eqElem,
    ordElem: ordElem,
    foldableElem: foldableElem,
    functorElem: functorElem,
    traversableElem: traversableElem,
    eqKey: eqKey,
    showKey: showKey,
    semigroupKey: semigroupKey,
    ordKey: ordKey,
    monoidKey: monoidKey,
    measuredElemKey: measuredElemKey
};