"use strict";
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Sequence = require("../Data.Sequence");
var Data_Tuple = require("../Data.Tuple");
var Data_Unfoldable = require("../Data.Unfoldable");
var Partial = require("../Partial");
var Prelude = require("../Prelude");
var LimitQueue = function (x) {
    return x;
};
var toArray = function (v) {
    return Data_Sequence.toUnfoldable(Data_Functor.functorArray)(Data_Unfoldable.unfoldableArray)(v.seq);
};
var insert = function (x) {
    return function (v) {
        var $8 = Data_Sequence.length(v.seq) < v.limit;
        if ($8) {
            return LimitQueue((function () {
                var $9 = {};
                for (var $10 in v) {
                    if ({}.hasOwnProperty.call(v, $10)) {
                        $9[$10] = v[$10];
                    };
                };
                $9.seq = Data_Sequence.cons(x)(v.seq);
                return $9;
            })());
        };
        return LimitQueue((function () {
            var $12 = {};
            for (var $13 in v) {
                if ({}.hasOwnProperty.call(v, $13)) {
                    $12[$13] = v[$13];
                };
            };
            $12.seq = Data_Maybe.maybe(Data_Sequence.empty)(Data_Tuple.fst)(Data_Sequence.unsnoc(Data_Sequence.cons(x)(v.seq)));
            return $12;
        })());
    };
};
var empty = function (dictPartial) {
    return function (n) {
        var $15 = n < 0;
        if ($15) {
            return Partial.crashWith(dictPartial)("Can't have a negative sized LimitQueue");
        };
        return {
            limit: n,
            seq: Data_Sequence.empty
        };
    };
};
module.exports = {
    LimitQueue: LimitQueue,
    empty: empty,
    insert: insert,
    toArray: toArray
};
