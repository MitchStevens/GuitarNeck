// Generated by purs version 0.11.7
"use strict";
var Control_Bind = require("../Control.Bind");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Test_Unit = require("../Test.Unit");
var expectFailure = function (reason) {
    return function (t) {
        return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Aff.attempt(t))(function (v) {
            return Data_Either.either(Data_Function["const"](Test_Unit.success))(Data_Function["const"](Test_Unit.failure(reason)))(v);
        });
    };
};
var equal$prime = function (dictEq) {
    return function (reason) {
        return function (expected) {
            return function (actual) {
                var $14 = Data_Eq.eq(dictEq)(expected)(actual);
                if ($14) {
                    return Test_Unit.success;
                };
                return Test_Unit.failure(reason);
            };
        };
    };
};
var equal = function (dictEq) {
    return function (dictShow) {
        return function (expected) {
            return function (actual) {
                var $15 = Data_Eq.eq(dictEq)(expected)(actual);
                if ($15) {
                    return Test_Unit.success;
                };
                return Test_Unit.failure("expected " + (Data_Show.show(dictShow)(expected) + (", got " + Data_Show.show(dictShow)(actual))));
            };
        };
    };
};
var equal$prime$prime = function (dictEq) {
    return function (dictShow) {
        return function (name) {
            return function (a) {
                return function (b) {
                    return Control_Monad_Error_Class.catchError(Control_Monad_Aff.monadErrorAff)(equal(dictEq)(dictShow)(a)(b))(function ($20) {
                        return Control_Monad_Error_Class.throwError(Control_Monad_Aff.monadThrowAff)(Control_Monad_Eff_Exception.error((function (v) {
                            return name + " " + v;
                        })(Control_Monad_Eff_Exception.message($20))));
                    });
                };
            };
        };
    };
};
var shouldEqual = function (dictEq) {
    return function (dictShow) {
        return Data_Function.flip(equal(dictEq)(dictShow));
    };
};
var assertFalse = function (v) {
    return function (v1) {
        if (!v1) {
            return Test_Unit.success;
        };
        if (v1) {
            return Test_Unit.failure(v);
        };
        throw new Error("Failed pattern match at Test.Unit.Assert line 26, column 1 - line 26, column 53: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var assert = function (v) {
    return function (v1) {
        if (v1) {
            return Test_Unit.success;
        };
        if (!v1) {
            return Test_Unit.failure(v);
        };
        throw new Error("Failed pattern match at Test.Unit.Assert line 20, column 1 - line 20, column 48: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
module.exports = {
    assert: assert,
    assertFalse: assertFalse,
    expectFailure: expectFailure,
    equal: equal,
    "equal'": equal$prime,
    shouldEqual: shouldEqual
};