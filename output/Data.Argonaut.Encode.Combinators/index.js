// Generated by purs version 0.11.7
"use strict";
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Argonaut_Core = require("../Data.Argonaut.Core");
var Data_Argonaut_Encode_Class = require("../Data.Argonaut.Encode.Class");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_StrMap = require("../Data.StrMap");
var Data_Tuple = require("../Data.Tuple");
var Prelude = require("../Prelude");
var extend = function (dictEncodeJson) {
    return function (v) {
        return function ($11) {
            return Data_Argonaut_Core.foldJsonObject(Data_Argonaut_Core.jsonSingletonObject(v.value0)(v.value1))(function ($12) {
                return Data_Argonaut_Core.fromObject(Data_StrMap.insert(v.value0)(v.value1)($12));
            })(Data_Argonaut_Encode_Class.encodeJson(dictEncodeJson)($11));
        };
    };
};
var extendOptional = function (dictEncodeJson) {
    return function (v) {
        if (v instanceof Data_Maybe.Just) {
            return extend(dictEncodeJson)(v.value0);
        };
        if (v instanceof Data_Maybe.Nothing) {
            return Data_Argonaut_Encode_Class.encodeJson(dictEncodeJson);
        };
        throw new Error("Failed pattern match at Data.Argonaut.Encode.Combinators line 49, column 1 - line 49, column 70: " + [ v.constructor.name ]);
    };
};
var assoc = function (dictEncodeJson) {
    return function (k) {
        return function ($13) {
            return Data_Tuple.Tuple.create(k)(Data_Argonaut_Encode_Class.encodeJson(dictEncodeJson)($13));
        };
    };
};
var assocOptional = function (dictEncodeJson) {
    return function (k) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(function ($14) {
            return assoc(Data_Argonaut_Encode_Class.encodeJsonJson)(k)(Data_Argonaut_Encode_Class.encodeJson(dictEncodeJson)($14));
        });
    };
};
module.exports = {
    assoc: assoc,
    assocOptional: assocOptional,
    extend: extend,
    extendOptional: extendOptional
};