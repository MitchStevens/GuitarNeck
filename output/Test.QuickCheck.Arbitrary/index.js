// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Category = require("../Control.Category");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Gen_Class = require("../Control.Monad.Gen.Class");
var Control_Monad_Gen_Common = require("../Control.Monad.Gen.Common");
var Control_Monad_ST = require("../Control.Monad.ST");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array_NonEmpty = require("../Data.Array.NonEmpty");
var Data_Array_ST = require("../Data.Array.ST");
var Data_Char = require("../Data.Char");
var Data_Either = require("../Data.Either");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Generic_Rep = require("../Data.Generic.Rep");
var Data_Identity = require("../Data.Identity");
var Data_Int = require("../Data.Int");
var Data_Lazy = require("../Data.Lazy");
var Data_List = require("../Data.List");
var Data_List_NonEmpty = require("../Data.List.NonEmpty");
var Data_List_Types = require("../Data.List.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_Newtype = require("../Data.Newtype");
var Data_NonEmpty = require("../Data.NonEmpty");
var Data_Ordering = require("../Data.Ordering");
var Data_Record = require("../Data.Record");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_String = require("../Data.String");
var Data_String_NonEmpty = require("../Data.String.NonEmpty");
var Data_Symbol = require("../Data.Symbol");
var Data_Tuple = require("../Data.Tuple");
var Data_Unit = require("../Data.Unit");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var Test_QuickCheck_Gen = require("../Test.QuickCheck.Gen");
var Type_Prelude = require("../Type.Prelude");
var Type_Row = require("../Type.Row");
var Arbitrary = function (arbitrary) {
    this.arbitrary = arbitrary;
};
var Coarbitrary = function (coarbitrary) {
    this.coarbitrary = coarbitrary;
};
var ArbitraryGenericSum = function (arbitraryGenericSum) {
    this.arbitraryGenericSum = arbitraryGenericSum;
};
var ArbitraryRowList = function (arbitraryRecord) {
    this.arbitraryRecord = arbitraryRecord;
};
var coarbitraryNoArguments = new Coarbitrary(function (v) {
    return Control_Category.id(Control_Category.categoryFn);
});
var coarbitrary = function (dict) {
    return dict.coarbitrary;
};
var coarbitraryArgument = function (dictCoarbitrary) {
    return new Coarbitrary(function (v) {
        return coarbitrary(dictCoarbitrary)(v);
    });
};
var coarbitraryConstructor = function (dictCoarbitrary) {
    return new Coarbitrary(function (v) {
        return coarbitrary(dictCoarbitrary)(v);
    });
};
var coarbitraryField = function (dictCoarbitrary) {
    return new Coarbitrary(function (v) {
        return coarbitrary(dictCoarbitrary)(v);
    });
};
var coarbitraryProduct = function (dictCoarbitrary) {
    return function (dictCoarbitrary1) {
        return new Coarbitrary(function (v) {
            return function ($119) {
                return coarbitrary(dictCoarbitrary1)(v.value1)(coarbitrary(dictCoarbitrary)(v.value0)($119));
            };
        });
    };
};
var coarbitraryRec = function (dictCoarbitrary) {
    return new Coarbitrary(function (v) {
        return coarbitrary(dictCoarbitrary)(v);
    });
};
var coarbitrarySum = function (dictCoarbitrary) {
    return function (dictCoarbitrary1) {
        return new Coarbitrary(function (v) {
            if (v instanceof Data_Generic_Rep.Inl) {
                return coarbitrary(dictCoarbitrary)(v.value0);
            };
            if (v instanceof Data_Generic_Rep.Inr) {
                return coarbitrary(dictCoarbitrary1)(v.value0);
            };
            throw new Error("Failed pattern match at Test.QuickCheck.Arbitrary line 213, column 1 - line 213, column 83: " + [ v.constructor.name ]);
        });
    };
};
var genericCoarbitrary = function (dictGeneric) {
    return function (dictCoarbitrary) {
        return function (x) {
            return function (g) {
                return Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_Generic_Rep.to(dictGeneric))(coarbitrary(dictCoarbitrary)(Data_Generic_Rep.from(dictGeneric)(x))(Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_Generic_Rep.from(dictGeneric))(g)));
            };
        };
    };
};
var coarbUnit = new Coarbitrary(function (v) {
    return Test_QuickCheck_Gen.perturbGen(1.0);
});
var coarbTuple = function (dictCoarbitrary) {
    return function (dictCoarbitrary1) {
        return new Coarbitrary(function (v) {
            return function ($120) {
                return coarbitrary(dictCoarbitrary1)(v.value1)(coarbitrary(dictCoarbitrary)(v.value0)($120));
            };
        });
    };
};
var coarbOrdering = new Coarbitrary(function (v) {
    if (v instanceof Data_Ordering.LT) {
        return Test_QuickCheck_Gen.perturbGen(1.0);
    };
    if (v instanceof Data_Ordering.EQ) {
        return Test_QuickCheck_Gen.perturbGen(2.0);
    };
    if (v instanceof Data_Ordering.GT) {
        return Test_QuickCheck_Gen.perturbGen(3.0);
    };
    throw new Error("Failed pattern match at Test.QuickCheck.Arbitrary line 111, column 1 - line 111, column 47: " + [ v.constructor.name ]);
});
var coarbNumber = new Coarbitrary(Test_QuickCheck_Gen.perturbGen);
var coarbNonEmpty = function (dictCoarbitrary) {
    return function (dictCoarbitrary1) {
        return new Coarbitrary(function (v) {
            return function ($121) {
                return coarbitrary(dictCoarbitrary)(v.value1)(coarbitrary(dictCoarbitrary1)(v.value0)($121));
            };
        });
    };
};
var coarbMaybe = function (dictCoarbitrary) {
    return new Coarbitrary(function (v) {
        if (v instanceof Data_Maybe.Nothing) {
            return Test_QuickCheck_Gen.perturbGen(1.0);
        };
        if (v instanceof Data_Maybe.Just) {
            return coarbitrary(dictCoarbitrary)(v.value0);
        };
        throw new Error("Failed pattern match at Test.QuickCheck.Arbitrary line 151, column 1 - line 151, column 62: " + [ v.constructor.name ]);
    });
};
var coarbList = function (dictCoarbitrary) {
    return new Coarbitrary(Data_Foldable.foldl(Data_List_Types.foldableList)(function (f) {
        return function (x) {
            return function ($122) {
                return f(coarbitrary(dictCoarbitrary)(x)($122));
            };
        };
    })(Control_Category.id(Control_Category.categoryFn)));
};
var coarbNonEmptyList = function (dictCoarbitrary) {
    return new Coarbitrary(function (v) {
        return coarbitrary(coarbNonEmpty(coarbList(dictCoarbitrary))(dictCoarbitrary))(v);
    });
};
var coarbLazy = function (dictCoarbitrary) {
    return new Coarbitrary(function (a) {
        return coarbitrary(dictCoarbitrary)(Data_Lazy.force(a));
    });
};
var coarbInt = new Coarbitrary(function ($123) {
    return Test_QuickCheck_Gen.perturbGen(Data_Int.toNumber($123));
});
var coarbIdentity = function (dictCoarbitrary) {
    return new Coarbitrary(function (v) {
        return coarbitrary(dictCoarbitrary)(v);
    });
};
var coarbEither = function (dictCoarbitrary) {
    return function (dictCoarbitrary1) {
        return new Coarbitrary(function (v) {
            if (v instanceof Data_Either.Left) {
                return coarbitrary(dictCoarbitrary)(v.value0);
            };
            if (v instanceof Data_Either.Right) {
                return coarbitrary(dictCoarbitrary1)(v.value0);
            };
            throw new Error("Failed pattern match at Test.QuickCheck.Arbitrary line 158, column 1 - line 158, column 83: " + [ v.constructor.name ]);
        });
    };
};
var coarbChar = new Coarbitrary(function (c) {
    return coarbitrary(coarbInt)(Data_Char.toCharCode(c));
});
var coarbBoolean = new Coarbitrary(function (v) {
    if (v) {
        return Test_QuickCheck_Gen.perturbGen(1.0);
    };
    if (!v) {
        return Test_QuickCheck_Gen.perturbGen(2.0);
    };
    throw new Error("Failed pattern match at Test.QuickCheck.Arbitrary line 68, column 1 - line 68, column 45: " + [ v.constructor.name ]);
});
var coarbArray = function (dictCoarbitrary) {
    return new Coarbitrary(Data_Foldable.foldl(Data_Foldable.foldableArray)(function (f) {
        return function (x) {
            return function ($124) {
                return f(coarbitrary(dictCoarbitrary)(x)($124));
            };
        };
    })(Control_Category.id(Control_Category.categoryFn)));
};
var coarbNonEmptyArray = function (dictCoarbitrary) {
    return new Coarbitrary(function ($125) {
        return coarbitrary(coarbArray(dictCoarbitrary))(Data_Array_NonEmpty.toArray($125));
    });
};
var coarbString = new Coarbitrary(function (s) {
    return coarbitrary(coarbArray(coarbMaybe(coarbInt)))(Data_Functor.map(Data_Functor.functorArray)(Data_String.charCodeAt(0))(Data_String.split(Data_Newtype.wrap(Data_String.newtypePattern)(""))(s)));
});
var coarbNonEmptyString = new Coarbitrary(function ($126) {
    return coarbitrary(coarbString)(Data_String_NonEmpty.toString($126));
});
var arbitraryRowListNil = new ArbitraryRowList(function (v) {
    return Control_Applicative.pure(Test_QuickCheck_Gen.applicativeGen)({});
});
var arbitraryRecord = function (dict) {
    return dict.arbitraryRecord;
};
var arbitraryRecordInstance = function (dictRowToList) {
    return function (dictArbitraryRowList) {
        return new Arbitrary(arbitraryRecord(dictArbitraryRowList)(Type_Row.RLProxy.value));
    };
};
var arbitraryNoArguments = new Arbitrary(Control_Applicative.pure(Test_QuickCheck_Gen.applicativeGen)(Data_Generic_Rep.NoArguments.value));
var arbitraryGenericSum = function (dict) {
    return dict.arbitraryGenericSum;
};
var arbitrary = function (dict) {
    return dict.arbitrary;
};
var arbitraryArgument = function (dictArbitrary) {
    return new Arbitrary(Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_Generic_Rep.Argument)(arbitrary(dictArbitrary)));
};
var arbitraryConstructor = function (dictArbitrary) {
    return new Arbitrary(Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_Generic_Rep.Constructor)(arbitrary(dictArbitrary)));
};
var arbitraryField = function (dictArbitrary) {
    return new Arbitrary(Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_Generic_Rep.Field)(arbitrary(dictArbitrary)));
};
var arbitraryIdentity = function (dictArbitrary) {
    return new Arbitrary(Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_Identity.Identity)(arbitrary(dictArbitrary)));
};
var arbitraryLazy = function (dictArbitrary) {
    return new Arbitrary(Control_Bind.bind(Test_QuickCheck_Gen.bindGen)(arbitrary(dictArbitrary))(function ($127) {
        return Control_Applicative.pure(Test_QuickCheck_Gen.applicativeGen)(Data_Lazy.defer(Data_Function["const"]($127)));
    }));
};
var arbitraryList = function (dictArbitrary) {
    return new Arbitrary(Test_QuickCheck_Gen.sized(function (n) {
        return Control_Bind.bind(Test_QuickCheck_Gen.bindGen)(Test_QuickCheck_Gen.chooseInt(0)(n))(Data_Function.flip(Test_QuickCheck_Gen.listOf)(arbitrary(dictArbitrary)));
    }));
};
var arbitraryProduct = function (dictArbitrary) {
    return function (dictArbitrary1) {
        return new Arbitrary(Control_Apply.apply(Test_QuickCheck_Gen.applyGen)(Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_Generic_Rep.Product.create)(arbitrary(dictArbitrary)))(arbitrary(dictArbitrary1)));
    };
};
var arbitraryRec = function (dictArbitrary) {
    return new Arbitrary(Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_Generic_Rep.Rec)(arbitrary(dictArbitrary)));
};
var arbitraryRowListCons = function (dictArbitrary) {
    return function (dictArbitraryRowList) {
        return function (dictRowLacks) {
            return function (dictRowCons) {
                return function (dictRowToList) {
                    return function (dictIsSymbol) {
                        return new ArbitraryRowList(function (v) {
                            return Control_Bind.bind(Test_QuickCheck_Gen.bindGen)(arbitrary(dictArbitrary))(function (v1) {
                                return Control_Bind.bind(Test_QuickCheck_Gen.bindGen)(arbitraryRecord(dictArbitraryRowList)(Type_Row.RLProxy.value))(function (v2) {
                                    return Control_Applicative.pure(Test_QuickCheck_Gen.applicativeGen)(Data_Record.insert(dictIsSymbol)(dictRowLacks)(dictRowCons)(Data_Symbol.SProxy.value)(v1)(v2));
                                });
                            });
                        });
                    };
                };
            };
        };
    };
};
var arbitrarySum = function (dictArbitrary) {
    return function (dictArbitraryGenericSum) {
        return new Arbitrary(Test_QuickCheck_Gen.oneOf(new Data_NonEmpty.NonEmpty(Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_Generic_Rep.Inl.create)(arbitrary(dictArbitrary)), Data_Functor.map(Data_Functor.functorArray)(Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_Generic_Rep.Inr.create))(arbitraryGenericSum(dictArbitraryGenericSum)))));
    };
};
var genericArbitrary = function (dictGeneric) {
    return function (dictArbitrary) {
        return Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_Generic_Rep.to(dictGeneric))(arbitrary(dictArbitrary));
    };
};
var arbUnit = new Arbitrary(Control_Applicative.pure(Test_QuickCheck_Gen.applicativeGen)(Data_Unit.unit));
var arbTuple = function (dictArbitrary) {
    return function (dictArbitrary1) {
        return new Arbitrary(Control_Apply.apply(Test_QuickCheck_Gen.applyGen)(Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_Tuple.Tuple.create)(arbitrary(dictArbitrary)))(arbitrary(dictArbitrary1)));
    };
};
var arbOrdering = new Arbitrary(Test_QuickCheck_Gen.elements(new Data_NonEmpty.NonEmpty(Data_Ordering.LT.value, [ Data_Ordering.EQ.value, Data_Ordering.GT.value ])));
var arbNumber = new Arbitrary(Test_QuickCheck_Gen.uniform);
var arbNonEmpty = function (dictArbitrary) {
    return function (dictArbitrary1) {
        return new Arbitrary(Control_Apply.apply(Test_QuickCheck_Gen.applyGen)(Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_NonEmpty.NonEmpty.create)(arbitrary(dictArbitrary1)))(arbitrary(dictArbitrary)));
    };
};
var arbNonEmptyList = function (dictArbitrary) {
    return new Arbitrary(Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_List_Types.NonEmptyList)(arbitrary(arbNonEmpty(arbitraryList(dictArbitrary))(dictArbitrary))));
};
var arbMaybe = function (dictArbitrary) {
    return new Arbitrary(Control_Monad_Gen_Common.genMaybe(Test_QuickCheck_Gen.monadGenGen)(arbitrary(dictArbitrary)));
};
var arbInt = new Arbitrary(Test_QuickCheck_Gen.chooseInt(-1000000 | 0)(1000000));
var arbGenSumSum = function (dictArbitrary) {
    return function (dictArbitraryGenericSum) {
        return new ArbitraryGenericSum(Data_Semigroup.append(Data_Semigroup.semigroupArray)([ Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_Generic_Rep.Inl.create)(arbitrary(dictArbitrary)) ])(Data_Functor.map(Data_Functor.functorArray)(Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_Generic_Rep.Inr.create))(arbitraryGenericSum(dictArbitraryGenericSum))));
    };
};
var arbGenSumConstructor = function (dictArbitrary) {
    return new ArbitraryGenericSum([ arbitrary(arbitraryConstructor(dictArbitrary)) ]);
};
var arbFunction = function (dictCoarbitrary) {
    return function (dictArbitrary) {
        return new Arbitrary(Test_QuickCheck_Gen.repeatable(function (a) {
            return coarbitrary(dictCoarbitrary)(a)(arbitrary(dictArbitrary));
        }));
    };
};
var arbEither = function (dictArbitrary) {
    return function (dictArbitrary1) {
        return new Arbitrary(Control_Monad_Gen_Common.genEither(Test_QuickCheck_Gen.monadGenGen)(arbitrary(dictArbitrary))(arbitrary(dictArbitrary1)));
    };
};
var arbChar = new Arbitrary(Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_Char.fromCharCode)(Test_QuickCheck_Gen.chooseInt(0)(65536)));
var arbBoolean = new Arbitrary(Control_Monad_Gen_Class.chooseBool(Test_QuickCheck_Gen.monadGenGen));
var arbArray = function (dictArbitrary) {
    return new Arbitrary(Test_QuickCheck_Gen.arrayOf(arbitrary(dictArbitrary)));
};
var arbNonEmptyArray = function (dictArbitrary) {
    return new Arbitrary(Control_Bind.bind(Test_QuickCheck_Gen.bindGen)(arbitrary(dictArbitrary))(function (v) {
        return Control_Bind.bind(Test_QuickCheck_Gen.bindGen)(arbitrary(arbArray(dictArbitrary)))(function (v1) {
            return Control_Applicative.pure(Test_QuickCheck_Gen.applicativeGen)(Data_Maybe.fromJust()(Data_Array_NonEmpty.fromArray(Control_Monad_ST.pureST(function __do() {
                var v2 = Data_Array_ST.unsafeThaw(v1)();
                var v3 = Data_Array_ST.pushSTArray(v2)(v)();
                return Data_Array_ST.unsafeFreeze(v2)();
            }))));
        });
    }));
};
var arbString = new Arbitrary(Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_String.fromCharArray)(arbitrary(arbArray(arbChar))));
var arbNonEmptyString = new Arbitrary(Control_Apply.apply(Test_QuickCheck_Gen.applyGen)(Data_Functor.map(Test_QuickCheck_Gen.functorGen)(Data_String_NonEmpty.cons)(arbitrary(arbChar)))(arbitrary(arbString)));
var coarbFunction = function (dictArbitrary) {
    return function (dictCoarbitrary) {
        return new Coarbitrary(function (f) {
            return function (gen) {
                return Control_Bind.bind(Test_QuickCheck_Gen.bindGen)(arbitrary(arbArray(dictArbitrary)))(function (v) {
                    return coarbitrary(coarbArray(dictCoarbitrary))(Data_Functor.map(Data_Functor.functorArray)(f)(v))(gen);
                });
            };
        });
    };
};
module.exports = {
    Arbitrary: Arbitrary,
    arbitrary: arbitrary,
    Coarbitrary: Coarbitrary,
    coarbitrary: coarbitrary,
    genericArbitrary: genericArbitrary,
    genericCoarbitrary: genericCoarbitrary,
    ArbitraryGenericSum: ArbitraryGenericSum,
    arbitraryGenericSum: arbitraryGenericSum,
    ArbitraryRowList: ArbitraryRowList,
    arbitraryRecord: arbitraryRecord,
    arbBoolean: arbBoolean,
    coarbBoolean: coarbBoolean,
    arbNumber: arbNumber,
    coarbNumber: coarbNumber,
    arbInt: arbInt,
    coarbInt: coarbInt,
    arbString: arbString,
    coarbString: coarbString,
    arbNonEmptyString: arbNonEmptyString,
    coarbNonEmptyString: coarbNonEmptyString,
    arbChar: arbChar,
    coarbChar: coarbChar,
    arbUnit: arbUnit,
    coarbUnit: coarbUnit,
    arbOrdering: arbOrdering,
    coarbOrdering: coarbOrdering,
    arbArray: arbArray,
    coarbArray: coarbArray,
    arbNonEmptyArray: arbNonEmptyArray,
    coarbNonEmptyArray: coarbNonEmptyArray,
    arbFunction: arbFunction,
    coarbFunction: coarbFunction,
    arbTuple: arbTuple,
    coarbTuple: coarbTuple,
    arbMaybe: arbMaybe,
    coarbMaybe: coarbMaybe,
    arbEither: arbEither,
    coarbEither: coarbEither,
    arbitraryList: arbitraryList,
    coarbList: coarbList,
    arbitraryIdentity: arbitraryIdentity,
    coarbIdentity: coarbIdentity,
    arbitraryLazy: arbitraryLazy,
    coarbLazy: coarbLazy,
    arbNonEmpty: arbNonEmpty,
    coarbNonEmpty: coarbNonEmpty,
    arbNonEmptyList: arbNonEmptyList,
    coarbNonEmptyList: coarbNonEmptyList,
    arbitraryNoArguments: arbitraryNoArguments,
    coarbitraryNoArguments: coarbitraryNoArguments,
    arbGenSumSum: arbGenSumSum,
    arbGenSumConstructor: arbGenSumConstructor,
    arbitrarySum: arbitrarySum,
    coarbitrarySum: coarbitrarySum,
    arbitraryProduct: arbitraryProduct,
    coarbitraryProduct: coarbitraryProduct,
    arbitraryConstructor: arbitraryConstructor,
    coarbitraryConstructor: coarbitraryConstructor,
    arbitraryArgument: arbitraryArgument,
    coarbitraryArgument: coarbitraryArgument,
    arbitraryRec: arbitraryRec,
    coarbitraryRec: coarbitraryRec,
    arbitraryField: arbitraryField,
    coarbitraryField: coarbitraryField,
    arbitraryRowListNil: arbitraryRowListNil,
    arbitraryRowListCons: arbitraryRowListCons,
    arbitraryRecordInstance: arbitraryRecordInstance
};
