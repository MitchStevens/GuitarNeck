// Generated by purs version 0.11.7
"use strict";
var Data_Symbol = require("../Data.Symbol");
var Type_Data_Boolean = require("../Type.Data.Boolean");
var Type_Data_Ordering = require("../Type.Data.Ordering");
var CompareSymbol = {};
var AppendSymbol = {};
var Equals = {};
var equalsSymbol = function (dictCompareSymbol) {
    return function (dictEquals) {
        return Equals;
    };
};
var equals = function (dictEquals) {
    return function (v) {
        return function (v1) {
            return Type_Data_Boolean.BProxy.value;
        };
    };
};
var compareSymbol = function (dictCompareSymbol) {
    return function (v) {
        return function (v1) {
            return Type_Data_Ordering.OProxy.value;
        };
    };
};
var appendSymbol = function (dictAppendSymbol) {
    return function (v) {
        return function (v1) {
            return Data_Symbol.SProxy.value;
        };
    };
};
module.exports = {
    CompareSymbol: CompareSymbol,
    compareSymbol: compareSymbol,
    AppendSymbol: AppendSymbol,
    appendSymbol: appendSymbol,
    Equals: Equals,
    equals: equals,
    equalsSymbol: equalsSymbol
};