// Generated by purs version 0.11.7
"use strict";
var $foreign = require("./foreign");
var Control_Alt = require("../Control.Alt");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Lazy = require("../Control.Lazy");
var Control_Monad = require("../Control.Monad");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Random = require("../Control.Monad.Eff.Random");
var Control_Monad_Gen_Class = require("../Control.Monad.Gen.Class");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class");
var Control_Monad_State = require("../Control.Monad.State");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Bounded = require("../Data.Bounded");
var Data_Enum = require("../Data.Enum");
var Data_Eq = require("../Data.Eq");
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Identity = require("../Data.Identity");
var Data_Int = require("../Data.Int");
var Data_List = require("../Data.List");
var Data_List_Types = require("../Data.List.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid_Additive = require("../Data.Monoid.Additive");
var Data_Newtype = require("../Data.Newtype");
var Data_NonEmpty = require("../Data.NonEmpty");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var Data_Unit = require("../Data.Unit");
var $$Math = require("../Math");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var Test_QuickCheck_LCG = require("../Test.QuickCheck.LCG");
var Gen = function (x) {
    return x;
};
var unGen = function (v) {
    return v;
};
var runGen = function ($87) {
    return Control_Monad_State.runState(unGen($87));
};
var stateful = function (f) {
    return Gen(Control_Monad_State_Class.state(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(function (s) {
        return runGen(f(s))(s);
    }));
};
var sized = function (f) {
    return stateful(function (s) {
        return f(s.size);
    });
};
var variant = function (n) {
    return function (g) {
        return Gen(Control_Monad_State_Class.state(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(function (s) {
            return runGen(g)((function () {
                var $30 = {};
                for (var $31 in s) {
                    if ({}.hasOwnProperty.call(s, $31)) {
                        $30[$31] = s[$31];
                    };
                };
                $30.newSeed = n;
                return $30;
            })());
        }));
    };
};
var resize = function (sz) {
    return function (g) {
        return Gen(Control_Monad_State_Class.state(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(function (v) {
            return Data_Functor.map(Data_Tuple.functorTuple)(function (v1) {
                var $34 = {};
                for (var $35 in v1) {
                    if ({}.hasOwnProperty.call(v1, $35)) {
                        $34[$35] = v1[$35];
                    };
                };
                $34.size = v.size;
                return $34;
            })(runGen(g)({
                newSeed: v.newSeed,
                size: sz
            }));
        }));
    };
};
var replicateMRec = function (dictMonadRec) {
    return function (k) {
        return function (v) {
            if (k <= 0) {
                return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(Data_List_Types.Nil.value);
            };
            var go = function (v1) {
                if (v1.value1 === 0) {
                    return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Done(v1.value0));
                };
                return Data_Functor.mapFlipped((((dictMonadRec.Monad0()).Bind1()).Apply0()).Functor0())(v)(function (x) {
                    return new Control_Monad_Rec_Class.Loop(new Data_Tuple.Tuple(new Data_List_Types.Cons(x, v1.value0), v1.value1 - 1 | 0));
                });
            };
            return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go)(new Data_Tuple.Tuple(Data_List_Types.Nil.value, k));
        };
    };
};
var repeatable = function (f) {
    return Gen(Control_Monad_State_Class.state(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(function (s) {
        return new Data_Tuple.Tuple(function (a) {
            return Data_Tuple.fst(runGen(f(a))(s));
        }, (function () {
            var $46 = {};
            for (var $47 in s) {
                if ({}.hasOwnProperty.call(s, $47)) {
                    $46[$47] = s[$47];
                };
            };
            $46.newSeed = Test_QuickCheck_LCG.lcgNext(s.newSeed);
            return $46;
        })());
    }));
};
var perturbGen = function (n) {
    return function (gen) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_State_Trans.bindStateT(Data_Identity.monadIdentity))(Control_Monad_State_Class.modify(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(function (s) {
            var $49 = {};
            for (var $50 in s) {
                if ({}.hasOwnProperty.call(s, $50)) {
                    $49[$50] = s[$50];
                };
            };
            $49.newSeed = Test_QuickCheck_LCG.lcgPerturb(Data_Int.toNumber($foreign.float32ToInt32(n)))(s.newSeed);
            return $49;
        }))(function () {
            return unGen(gen);
        });
    };
};
var monadRecGen = Control_Monad_State_Trans.monadRecStateT(Control_Monad_Rec_Class.monadRecIdentity);
var monadGen = Control_Monad_State_Trans.monadStateT(Data_Identity.monadIdentity);
var listOf = replicateMRec(monadRecGen);
var lcgStep = (function () {
    var f = function (s) {
        return new Data_Tuple.Tuple(Test_QuickCheck_LCG.runSeed(s.newSeed), (function () {
            var $52 = {};
            for (var $53 in s) {
                if ({}.hasOwnProperty.call(s, $53)) {
                    $52[$53] = s[$53];
                };
            };
            $52.newSeed = Test_QuickCheck_LCG.lcgNext(s.newSeed);
            return $52;
        })());
    };
    return Gen(Control_Monad_State_Class.state(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(f));
})();
var lazyGen = Control_Monad_State_Trans.lazyStateT;
var functorGen = Control_Monad_State_Trans.functorStateT(Data_Identity.functorIdentity);
var uniform = Data_Functor.map(functorGen)(function (n) {
    return Data_Int.toNumber(n) / Data_Int.toNumber(Test_QuickCheck_LCG.lcgN);
})(lcgStep);
var vectorOf = function (k) {
    return function (g) {
        return Data_Functor.map(functorGen)(Data_List.toUnfoldable(Data_Unfoldable.unfoldableArray))(listOf(k)(g));
    };
};
var evalGen = function ($88) {
    return Control_Monad_State.evalState(unGen($88));
};
var sample = function (seed) {
    return function (sz) {
        return function (g) {
            return evalGen(vectorOf(sz)(g))({
                newSeed: seed,
                size: sz
            });
        };
    };
};
var randomSample$prime = function (n) {
    return function (g) {
        return function __do() {
            var v = Test_QuickCheck_LCG.randomSeed();
            return sample(v)(n)(g);
        };
    };
};
var randomSample = randomSample$prime(10);
var choose = function (a) {
    return function (b) {
        var min$prime = Data_Ord.min(Data_Ord.ordNumber)(a)(b);
        var max$prime = Data_Ord.max(Data_Ord.ordNumber)(a)(b);
        return Data_Functor.map(functorGen)(function ($89) {
            return min$prime + (max$prime - min$prime) * $89;
        })(uniform);
    };
};
var bindGen = Control_Monad_State_Trans.bindStateT(Data_Identity.monadIdentity);
var frequency = function (v) {
    var xxs = new Data_List_Types.Cons(v.value0, v.value1);
    var total = Data_Newtype.unwrap(Data_Monoid_Additive.newtypeAdditive)(Data_Foldable.fold(Data_List_Types.foldableList)(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringNumber))(Data_Functor.map(Data_List_Types.functorList)(function ($90) {
        return Data_Monoid_Additive.Additive(Data_Tuple.fst($90));
    })(xxs)));
    var pick = function ($copy_n) {
        return function ($copy_d) {
            return function ($copy_v1) {
                var $tco_var_n = $copy_n;
                var $tco_var_d = $copy_d;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(n, d, v1) {
                    if (v1 instanceof Data_List_Types.Nil) {
                        $tco_done = true;
                        return d;
                    };
                    if (v1 instanceof Data_List_Types.Cons) {
                        var $60 = n <= v1.value0.value0;
                        if ($60) {
                            $tco_done = true;
                            return v1.value0.value1;
                        };
                        $tco_var_n = n - v1.value0.value0;
                        $tco_var_d = d;
                        $copy_v1 = v1.value1;
                        return;
                    };
                    throw new Error("Failed pattern match at Test.QuickCheck.Gen line 163, column 5 - line 163, column 21: " + [ n.constructor.name, d.constructor.name, v1.constructor.name ]);
                };
                while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_n, $tco_var_d, $copy_v1);
                };
                return $tco_result;
            };
        };
    };
    return Control_Bind.bind(bindGen)(choose(0)(total))(function (v1) {
        return pick(v1)(Data_Tuple.snd(v.value0))(xxs);
    });
};
var applyGen = Control_Monad_State_Trans.applyStateT(Data_Identity.monadIdentity);
var chooseInt$prime = function (a) {
    return function (b) {
        var numB = Data_Int.toNumber(b);
        var numA = Data_Int.toNumber(a);
        var clamp = function (x) {
            return numA + $$Math.remainder(x)((numB - numA) + 1);
        };
        var choose31BitPosNumber = Data_Functor.map(functorGen)(Data_Int.toNumber)(lcgStep);
        var choose32BitPosNumber = Control_Apply.apply(applyGen)(Data_Functor.map(functorGen)(Data_Semiring.add(Data_Semiring.semiringNumber))(choose31BitPosNumber))(Data_Functor.map(functorGen)(Data_Semiring.mul(Data_Semiring.semiringNumber)(2.0))(choose31BitPosNumber));
        return Data_Functor.map(functorGen)(function ($91) {
            return Data_Int.floor(clamp($91));
        })(choose32BitPosNumber);
    };
};
var chooseInt = function (a) {
    return function (b) {
        var $68 = a <= b;
        if ($68) {
            return chooseInt$prime(a)(b);
        };
        return chooseInt$prime(b)(a);
    };
};
var arrayOf = function (g) {
    return sized(function (n) {
        return Control_Bind.bind(bindGen)(chooseInt(0)(n))(function (v) {
            return vectorOf(v)(g);
        });
    });
};
var monadGenGen = new Control_Monad_Gen_Class.MonadGen(function () {
    return monadGen;
}, Data_Functor.map(functorGen)(function (v) {
    return v < 0.5;
})(uniform), choose, chooseInt, function (f) {
    return function (g) {
        return sized(function (s) {
            return resize(f(s))(g);
        });
    };
}, sized);
var oneOf = function (v) {
    return Control_Bind.bind(bindGen)(chooseInt(0)(Data_Array.length(v.value1)))(function (v1) {
        var $72 = v1 < 1;
        if ($72) {
            return v.value0;
        };
        return Data_Maybe.fromMaybe(v.value0)(Data_Array.index(v.value1)(v1 - 1 | 0));
    });
};
var applicativeGen = Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity);
var arrayOf1 = function (g) {
    return sized(function (n) {
        return Control_Bind.bind(bindGen)(chooseInt(0)(n))(function (v) {
            return Control_Bind.bind(bindGen)(g)(function (v1) {
                return Control_Bind.bind(bindGen)(vectorOf(v - 1 | 0)(g))(function (v2) {
                    return Control_Applicative.pure(applicativeGen)(new Data_NonEmpty.NonEmpty(v1, v2));
                });
            });
        });
    });
};
var elements = function (v) {
    return Control_Bind.bind(bindGen)(chooseInt(0)(Data_Array.length(v.value1)))(function (v1) {
        return Control_Applicative.pure(applicativeGen)((function () {
            var $80 = v1 === 0;
            if ($80) {
                return v.value0;
            };
            return Data_Maybe.fromMaybe(v.value0)(Data_Array.index(v.value1)(v1 - 1 | 0));
        })());
    });
};
var $$enum = function (dictBoundedEnum) {
    return Control_Bind.bind(bindGen)(chooseInt(Data_Enum.fromEnum(dictBoundedEnum)(Data_Bounded.bottom(dictBoundedEnum.Bounded0())))(Data_Enum.fromEnum(dictBoundedEnum)(Data_Bounded.top(dictBoundedEnum.Bounded0()))))(function (v) {
        return Control_Applicative.pure(applicativeGen)(Data_Maybe.fromJust()(Data_Enum.toEnum(dictBoundedEnum)(v)));
    });
};
var shuffle = function (xs) {
    return Control_Bind.bind(bindGen)(vectorOf(Data_Array.length(xs))(chooseInt(0)(Data_Bounded.top(Data_Bounded.boundedInt))))(function (v) {
        return Control_Applicative.pure(applicativeGen)(Data_Functor.map(Data_Functor.functorArray)(Data_Tuple.snd)(Data_Array.sortBy(Data_Ord.comparing(Data_Ord.ordInt)(Data_Tuple.fst))(Data_Array.zip(v)(xs))));
    });
};
var suchThat = function (gen) {
    return function (pred) {
        var go = function (v) {
            return Control_Bind.bind(bindGen)(gen)(function (v1) {
                return Control_Applicative.pure(applicativeGen)((function () {
                    var $86 = pred(v1);
                    if ($86) {
                        return new Control_Monad_Rec_Class.Done(v1);
                    };
                    return new Control_Monad_Rec_Class.Loop(Data_Unit.unit);
                })());
            });
        };
        return Control_Monad_Rec_Class.tailRecM(monadRecGen)(go)(Data_Unit.unit);
    };
};
var altGen = Control_Monad_State_Trans.altStateT(Data_Identity.monadIdentity)(Data_Identity.altIdentity);
module.exports = {
    unGen: unGen,
    repeatable: repeatable,
    stateful: stateful,
    variant: variant,
    suchThat: suchThat,
    sized: sized,
    resize: resize,
    choose: choose,
    chooseInt: chooseInt,
    oneOf: oneOf,
    frequency: frequency,
    arrayOf: arrayOf,
    arrayOf1: arrayOf1,
    listOf: listOf,
    vectorOf: vectorOf,
    elements: elements,
    shuffle: shuffle,
    runGen: runGen,
    evalGen: evalGen,
    perturbGen: perturbGen,
    uniform: uniform,
    sample: sample,
    randomSample: randomSample,
    "randomSample'": randomSample$prime,
    functorGen: functorGen,
    applyGen: applyGen,
    applicativeGen: applicativeGen,
    bindGen: bindGen,
    monadGen: monadGen,
    altGen: altGen,
    monadRecGen: monadRecGen,
    lazyGen: lazyGen,
    monadGenGen: monadGenGen
};
