// Generated by purs version 0.11.7
"use strict";
var Control_Algebra_Properties = require("../Control.Algebra.Properties");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Lattice = require("../Data.Lattice");
var Prelude = require("../Prelude");
var Type_Proxy = require("../Type.Proxy");
var verifySemilattice = function (dictEq) {
    return function (f) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return Control_Algebra_Properties.associative(dictEq)(f)(a)(b)(c) && (Control_Algebra_Properties.commutative(dictEq)(f)(a)(b) && Control_Algebra_Properties["idempotent'"](dictEq)(f)(a));
                };
            };
        };
    };
};
var verifyMeetSemilattice = function (dictMeetSemilattice) {
    return function (dictEq) {
        return verifySemilattice(dictEq)(Data_Lattice.meet(dictMeetSemilattice));
    };
};
var verifyJoinSemilattice = function (dictJoinSemilattice) {
    return function (dictEq) {
        return verifySemilattice(dictEq)(Data_Lattice.join(dictJoinSemilattice));
    };
};
var verifyLattice = function (dictLattice) {
    return function (dictEq) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return verifyJoinSemilattice(dictLattice.JoinSemilattice0())(dictEq)(a)(b)(c) && (verifyMeetSemilattice(dictLattice.MeetSemilattice1())(dictEq)(a)(b)(c) && Control_Algebra_Properties.absorbtion(dictEq)(Data_Lattice.join(dictLattice.JoinSemilattice0()))(Data_Lattice.meet(dictLattice.MeetSemilattice1()))(a)(b));
                };
            };
        };
    };
};
var verifyBoundedMeetSemilattice = function (dictBoundedMeetSemilattice) {
    return function (dictEq) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return verifyMeetSemilattice(dictBoundedMeetSemilattice.MeetSemilattice0())(dictEq)(a)(b)(c) && Control_Algebra_Properties.identity(dictEq)(Data_Lattice.meet(dictBoundedMeetSemilattice.MeetSemilattice0()))(Data_Lattice.top(dictBoundedMeetSemilattice))(a);
                };
            };
        };
    };
};
var verifyBoundedJoinSemilattice = function (dictBoundedJoinSemilattice) {
    return function (dictEq) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return verifyJoinSemilattice(dictBoundedJoinSemilattice.JoinSemilattice0())(dictEq)(a)(b)(c) && Control_Algebra_Properties.identity(dictEq)(Data_Lattice.join(dictBoundedJoinSemilattice.JoinSemilattice0()))(Data_Lattice.bottom(dictBoundedJoinSemilattice))(a);
                };
            };
        };
    };
};
var verifyBoundedLattice = function (dictBoundedLattice) {
    return function (dictEq) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return verifyBoundedJoinSemilattice(dictBoundedLattice.BoundedJoinSemilattice0())(dictEq)(a)(b)(c) && (verifyBoundedMeetSemilattice(dictBoundedLattice.BoundedMeetSemilattice1())(dictEq)(a)(b)(c) && Control_Algebra_Properties.absorbtion(dictEq)(Data_Lattice.join((dictBoundedLattice.BoundedJoinSemilattice0()).JoinSemilattice0()))(Data_Lattice.meet((dictBoundedLattice.BoundedMeetSemilattice1()).MeetSemilattice0()))(a)(b));
                };
            };
        };
    };
};
var verify = function (dictEq) {
    return function (v) {
        return function (p) {
            return p;
        };
    };
};
module.exports = {
    verifySemilattice: verifySemilattice,
    verifyJoinSemilattice: verifyJoinSemilattice,
    verifyMeetSemilattice: verifyMeetSemilattice,
    verifyBoundedJoinSemilattice: verifyBoundedJoinSemilattice,
    verifyBoundedMeetSemilattice: verifyBoundedMeetSemilattice,
    verifyBoundedLattice: verifyBoundedLattice,
    verifyLattice: verifyLattice,
    verify: verify
};
