// Generated by purs version 0.11.7
"use strict";
var CSS_Common = require("../CSS.Common");
var CSS_Property = require("../CSS.Property");
var CSS_String = require("../CSS.String");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Generic = require("../Data.Generic");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ordering = require("../Data.Ordering");
var Data_Show = require("../Data.Show");
var Inside = (function () {
    function Inside() {

    };
    Inside.value = new Inside();
    return Inside;
})();
var Outside = (function () {
    function Outside() {

    };
    Outside.value = new Outside();
    return Outside;
})();
var Inherit = (function () {
    function Inherit() {

    };
    Inherit.value = new Inherit();
    return Inherit;
})();
var Initial = (function () {
    function Initial() {

    };
    Initial.value = new Initial();
    return Initial;
})();
var Unset = (function () {
    function Unset() {

    };
    Unset.value = new Unset();
    return Unset;
})();
var valListStylePosition = new CSS_Property.Val(function (v) {
    if (v instanceof Inside) {
        return CSS_String.fromString(CSS_Property.isStringValue)("inside");
    };
    if (v instanceof Outside) {
        return CSS_String.fromString(CSS_Property.isStringValue)("outside");
    };
    if (v instanceof Inherit) {
        return CSS_String.fromString(CSS_Property.isStringValue)("inherit");
    };
    if (v instanceof Initial) {
        return CSS_String.fromString(CSS_Property.isStringValue)("initial");
    };
    if (v instanceof Unset) {
        return CSS_String.fromString(CSS_Property.isStringValue)("unset");
    };
    throw new Error("Failed pattern match at CSS.ListStyle.Position line 27, column 1 - line 27, column 55: " + [ v.constructor.name ]);
});
var unsetListStylePosition = new CSS_Common.Unset(Unset.value);
var outside = Outside.value;
var listStylePosition = CSS_Stylesheet.key(valListStylePosition)(CSS_String.fromString(CSS_Property.isStringKey)("list-style-position"));
var inside = Inside.value;
var initialListStylePosition = new CSS_Common.Initial(Initial.value);
var inheritListStylePosition = new CSS_Common.Inherit(Inherit.value);
var genericListStylePosition = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.ListStyle.Position.Inside" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Inside.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.ListStyle.Position.Outside" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Outside.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.ListStyle.Position.Inherit" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Inherit.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.ListStyle.Position.Initial" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Initial.value);
    };
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.ListStyle.Position.Unset" && v.value1.length === 0)) {
        return new Data_Maybe.Just(Unset.value);
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.ListStyle.Position.ListStylePosition", [ {
        sigConstructor: "CSS.ListStyle.Position.Inside",
        sigValues: [  ]
    }, {
        sigConstructor: "CSS.ListStyle.Position.Outside",
        sigValues: [  ]
    }, {
        sigConstructor: "CSS.ListStyle.Position.Inherit",
        sigValues: [  ]
    }, {
        sigConstructor: "CSS.ListStyle.Position.Initial",
        sigValues: [  ]
    }, {
        sigConstructor: "CSS.ListStyle.Position.Unset",
        sigValues: [  ]
    } ]);
}, function (v) {
    if (v instanceof Inside) {
        return new Data_Generic.SProd("CSS.ListStyle.Position.Inside", [  ]);
    };
    if (v instanceof Outside) {
        return new Data_Generic.SProd("CSS.ListStyle.Position.Outside", [  ]);
    };
    if (v instanceof Inherit) {
        return new Data_Generic.SProd("CSS.ListStyle.Position.Inherit", [  ]);
    };
    if (v instanceof Initial) {
        return new Data_Generic.SProd("CSS.ListStyle.Position.Initial", [  ]);
    };
    if (v instanceof Unset) {
        return new Data_Generic.SProd("CSS.ListStyle.Position.Unset", [  ]);
    };
    throw new Error("Failed pattern match at CSS.ListStyle.Position line 22, column 8 - line 22, column 70: " + [ v.constructor.name ]);
});
var showListStylePosition = new Data_Show.Show(Data_Generic.gShow(genericListStylePosition));
var eqListStylePosition = new Data_Eq.Eq(function (x) {
    return function (y) {
        if (x instanceof Inside && y instanceof Inside) {
            return true;
        };
        if (x instanceof Outside && y instanceof Outside) {
            return true;
        };
        if (x instanceof Inherit && y instanceof Inherit) {
            return true;
        };
        if (x instanceof Initial && y instanceof Initial) {
            return true;
        };
        if (x instanceof Unset && y instanceof Unset) {
            return true;
        };
        return false;
    };
});
var ordListStylePosition = new Data_Ord.Ord(function () {
    return eqListStylePosition;
}, function (x) {
    return function (y) {
        if (x instanceof Inside && y instanceof Inside) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Inside) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Inside) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Outside && y instanceof Outside) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Outside) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Outside) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Inherit && y instanceof Inherit) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Inherit) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Inherit) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Initial && y instanceof Initial) {
            return Data_Ordering.EQ.value;
        };
        if (x instanceof Initial) {
            return Data_Ordering.LT.value;
        };
        if (y instanceof Initial) {
            return Data_Ordering.GT.value;
        };
        if (x instanceof Unset && y instanceof Unset) {
            return Data_Ordering.EQ.value;
        };
        throw new Error("Failed pattern match at CSS.ListStyle.Position line 21, column 8 - line 21, column 62: " + [ x.constructor.name, y.constructor.name ]);
    };
});
module.exports = {
    Inside: Inside,
    Outside: Outside,
    Inherit: Inherit,
    Initial: Initial,
    Unset: Unset,
    inside: inside,
    outside: outside,
    listStylePosition: listStylePosition,
    eqListStylePosition: eqListStylePosition,
    ordListStylePosition: ordListStylePosition,
    genericListStylePosition: genericListStylePosition,
    showListStylePosition: showListStylePosition,
    valListStylePosition: valListStylePosition,
    initialListStylePosition: initialListStylePosition,
    inheritListStylePosition: inheritListStylePosition,
    unsetListStylePosition: unsetListStylePosition
};