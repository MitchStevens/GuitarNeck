// Generated by purs version 0.11.7
"use strict";
var Data_Eq = require("../Data.Eq");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_MediaType = require("../Data.MediaType");
var Data_Newtype = require("../Data.Newtype");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Prelude = require("../Prelude");
var Accept = (function () {
    function Accept(value0) {
        this.value0 = value0;
    };
    Accept.create = function (value0) {
        return new Accept(value0);
    };
    return Accept;
})();
var ContentType = (function () {
    function ContentType(value0) {
        this.value0 = value0;
    };
    ContentType.create = function (value0) {
        return new ContentType(value0);
    };
    return ContentType;
})();
var RequestHeader = (function () {
    function RequestHeader(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    RequestHeader.create = function (value0) {
        return function (value1) {
            return new RequestHeader(value0, value1);
        };
    };
    return RequestHeader;
})();
var showRequestHeader = new Data_Show.Show(function (v) {
    if (v instanceof Accept) {
        return "(Accept " + (Data_Show.show(Data_MediaType.showMediaType)(v.value0) + ")");
    };
    if (v instanceof ContentType) {
        return "(ContentType " + (Data_Show.show(Data_MediaType.showMediaType)(v.value0) + ")");
    };
    if (v instanceof RequestHeader) {
        return "(RequestHeader " + (Data_Show.show(Data_Show.showString)(v.value0) + (" " + (Data_Show.show(Data_Show.showString)(v.value1) + ")")));
    };
    throw new Error("Failed pattern match at Network.HTTP.RequestHeader line 19, column 1 - line 19, column 49: " + [ v.constructor.name ]);
});
var requestHeaderValue = function (v) {
    if (v instanceof Accept) {
        return Data_Newtype.unwrap(Data_MediaType.newtypeMediaType)(v.value0);
    };
    if (v instanceof ContentType) {
        return Data_Newtype.unwrap(Data_MediaType.newtypeMediaType)(v.value0);
    };
    if (v instanceof RequestHeader) {
        return v.value1;
    };
    throw new Error("Failed pattern match at Network.HTTP.RequestHeader line 29, column 1 - line 29, column 46: " + [ v.constructor.name ]);
};
var requestHeaderName = function (v) {
    if (v instanceof Accept) {
        return "Accept";
    };
    if (v instanceof ContentType) {
        return "Content-Type";
    };
    if (v instanceof RequestHeader) {
        return v.value0;
    };
    throw new Error("Failed pattern match at Network.HTTP.RequestHeader line 24, column 1 - line 24, column 45: " + [ v.constructor.name ]);
};
var eqRequestHeader = new Data_Eq.Eq(function (v) {
    return function (v1) {
        if (v instanceof Accept && v1 instanceof Accept) {
            return Data_Eq.eq(Data_MediaType.eqMediaType)(v.value0)(v1.value0);
        };
        if (v instanceof ContentType && v1 instanceof ContentType) {
            return Data_Eq.eq(Data_MediaType.eqMediaType)(v.value0)(v1.value0);
        };
        if (v instanceof RequestHeader && v1 instanceof RequestHeader) {
            return v.value0 === v1.value0 && v.value1 === v1.value1;
        };
        return false;
    };
});
module.exports = {
    Accept: Accept,
    ContentType: ContentType,
    RequestHeader: RequestHeader,
    requestHeaderName: requestHeaderName,
    requestHeaderValue: requestHeaderValue,
    eqRequestHeader: eqRequestHeader,
    showRequestHeader: showRequestHeader
};
