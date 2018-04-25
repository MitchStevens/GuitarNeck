// Generated by purs version 0.11.7
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Category = require("../Control.Category");
var Control_Monad = require("../Control.Monad");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Sequence = require("../Data.Sequence");
var Data_Show = require("../Data.Show");
var Data_Traversable = require("../Data.Traversable");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var Seq = (function () {
    function Seq(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Seq.create = function (value0) {
        return function (value1) {
            return new Seq(value0, value1);
        };
    };
    return Seq;
})();
var unsnoc = function (v) {
    var v1 = Data_Sequence.unsnoc(v.value1);
    if (v1 instanceof Data_Maybe.Nothing) {
        return new Data_Tuple.Tuple(Data_Sequence.empty, v.value0);
    };
    if (v1 instanceof Data_Maybe.Just) {
        return new Data_Tuple.Tuple(Data_Sequence.cons(v.value0)(v1.value0.value0), v1.value0.value1);
    };
    throw new Error("Failed pattern match at Data.Sequence.NonEmpty line 99, column 3 - line 101, column 47: " + [ v1.constructor.name ]);
};
var uncons = function (v) {
    return new Data_Tuple.Tuple(v.value0, v.value1);
};
var toPlain = function (v) {
    return Data_Sequence.cons(v.value0)(v.value1);
};
var toUnfoldable = function (dictFunctor) {
    return function (dictUnfoldable) {
        return function ($123) {
            return Data_Sequence.toUnfoldable(dictFunctor)(dictUnfoldable)(toPlain($123));
        };
    };
};
var take = function (i) {
    return function ($124) {
        return Data_Sequence.take(i)(toPlain($124));
    };
};
var tail = function (v) {
    return v.value1;
};
var splitAt = function (i) {
    return function ($125) {
        return Data_Sequence.splitAt(i)(toPlain($125));
    };
};
var snoc = function (v) {
    return function (y) {
        return new Seq(v.value0, Data_Sequence.snoc(v.value1)(y));
    };
};
var singleton = function (x) {
    return new Seq(x, Data_Sequence.empty);
};
var showSeq = function (dictShow) {
    return new Data_Show.Show(function (v) {
        return "(Seq " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(Data_Sequence.showSeq(dictShow))(v.value1) + ")")));
    });
};
var semigroupSeq = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return new Seq(v.value0, Data_Semigroup.append(Data_Sequence.semigroupSeq)(Data_Sequence.snoc(v.value1)(v1.value0))(v1.value1));
    };
});
var length = function (v) {
    return Data_Sequence.length(v.value1) + 1 | 0;
};
var last = function (v) {
    return Data_Maybe.maybe(v.value0)(Control_Category.id(Control_Category.categoryFn))(Data_Sequence.last(v.value1));
};
var init = function ($126) {
    return Data_Tuple.fst(unsnoc($126));
};
var index = function (v) {
    return function (v1) {
        if (v === 0) {
            return new Data_Maybe.Just(v1.value0);
        };
        return Data_Sequence.index(v - 1 | 0)(v1.value1);
    };
};
var inBounds = function (v) {
    return function (v1) {
        if (v === 0) {
            return true;
        };
        return Data_Sequence.inBounds(v - 1 | 0)(v1.value1);
    };
};
var head = function (v) {
    return v.value0;
};
var functorSeq = new Data_Functor.Functor(function (f) {
    return function (v) {
        return new Seq(f(v.value0), Data_Functor.map(Data_Sequence.functorSeq)(f)(v.value1));
    };
});
var fromPlain = function (dictPartial) {
    return function ($127) {
        return Data_Tuple.uncurry(Seq.create)(Data_Maybe.fromJust(dictPartial)(Data_Sequence.uncons($127)));
    };
};
var foldableSeq = new Data_Foldable.Foldable(function (dictMonoid) {
    return function (f) {
        return function ($128) {
            return Data_Foldable.foldMap(Data_Sequence.foldableSeq)(dictMonoid)(f)(toPlain($128));
        };
    };
}, function (f) {
    return function (z) {
        return function ($129) {
            return Data_Foldable.foldl(Data_Sequence.foldableSeq)(f)(z)(toPlain($129));
        };
    };
}, function (f) {
    return function (z) {
        return function ($130) {
            return Data_Foldable.foldr(Data_Sequence.foldableSeq)(f)(z)(toPlain($130));
        };
    };
});
var traversableSeq = new Data_Traversable.Traversable(function () {
    return foldableSeq;
}, function () {
    return functorSeq;
}, function (dictApplicative) {
    return function ($131) {
        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(fromPlain())(Data_Traversable.sequence(Data_Sequence.traversableSeq)(dictApplicative)(toPlain($131)));
    };
}, function (dictApplicative) {
    return function (f) {
        return function ($132) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())(fromPlain())(Data_Traversable.traverse(Data_Sequence.traversableSeq)(dictApplicative)(f)(toPlain($132)));
        };
    };
});
var filter = function (p) {
    return function ($133) {
        return Data_Sequence.filter(p)(toPlain($133));
    };
};
var eqSeq = function (dictEq) {
    return new Data_Eq.Eq(function (v) {
        return function (v1) {
            return Data_Eq.eq(dictEq)(v.value0)(v1.value0) && Data_Eq.eq(Data_Sequence.eqSeq(dictEq))(v.value1)(v1.value1);
        };
    });
};
var ordSeq = function (dictOrd) {
    return new Data_Ord.Ord(function () {
        return eqSeq(dictOrd.Eq0());
    }, function (v) {
        return function (v1) {
            var v2 = Data_Ord.compare(dictOrd)(v.value0)(v1.value0);
            if (v2 instanceof Data_Ordering.EQ) {
                return Data_Ord.compare(Data_Sequence.ordSeq(dictOrd))(v.value1)(v1.value1);
            };
            return v2;
        };
    });
};
var drop = function (i) {
    return function ($134) {
        return Data_Sequence.drop(i)(toPlain($134));
    };
};
var cons = function (x) {
    return function (v) {
        return new Seq(x, Data_Sequence.cons(v.value0)(v.value1));
    };
};
var applySeq = new Control_Apply.Apply(function () {
    return functorSeq;
}, function (fs) {
    return function (xs) {
        return fromPlain()(Control_Apply.apply(Data_Sequence.applySeq)(toPlain(fs))(toPlain(xs)));
    };
});
var bindSeq = new Control_Bind.Bind(function () {
    return applySeq;
}, function (xs) {
    return function (f) {
        return fromPlain()(Control_Bind.bind(Data_Sequence.bindSeq)(toPlain(xs))(function ($135) {
            return toPlain(f($135));
        }));
    };
});
var applicativeSeq = new Control_Applicative.Applicative(function () {
    return applySeq;
}, function (x) {
    return new Seq(x, Data_Sequence.empty);
});
var monadSeq = new Control_Monad.Monad(function () {
    return applicativeSeq;
}, function () {
    return bindSeq;
});
var append = function (v) {
    return function (v1) {
        return new Seq(v.value0, Data_Semigroup.append(Data_Sequence.semigroupSeq)(Data_Sequence.snoc(v.value1)(v1.value0))(v1.value1));
    };
};
var altSeq = new Control_Alt.Alt(function () {
    return functorSeq;
}, Data_Semigroup.append(semigroupSeq));
var adjust = function (f) {
    return function (v) {
        return function (v1) {
            if (v === 0) {
                return new Seq(f(v1.value0), v1.value1);
            };
            return new Seq(v1.value0, Data_Sequence.adjust(f)(v - 1 | 0)(v1.value1));
        };
    };
};
var replace = function (x) {
    return adjust(Data_Function["const"](x));
};
module.exports = {
    Seq: Seq,
    singleton: singleton,
    cons: cons,
    snoc: snoc,
    append: append,
    length: length,
    inBounds: inBounds,
    uncons: uncons,
    unsnoc: unsnoc,
    head: head,
    tail: tail,
    init: init,
    last: last,
    toPlain: toPlain,
    splitAt: splitAt,
    take: take,
    drop: drop,
    filter: filter,
    index: index,
    adjust: adjust,
    replace: replace,
    toUnfoldable: toUnfoldable,
    showSeq: showSeq,
    eqSeq: eqSeq,
    ordSeq: ordSeq,
    functorSeq: functorSeq,
    applySeq: applySeq,
    applicativeSeq: applicativeSeq,
    bindSeq: bindSeq,
    monadSeq: monadSeq,
    semigroupSeq: semigroupSeq,
    altSeq: altSeq,
    foldableSeq: foldableSeq,
    traversableSeq: traversableSeq
};