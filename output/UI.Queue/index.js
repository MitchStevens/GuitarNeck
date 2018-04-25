// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Data_Function = require("../Data.Function");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Semiring = require("../Data.Semiring");
var Data_Traversable = require("../Data.Traversable");
var Halogen = require("../Halogen");
var Halogen_Component = require("../Halogen.Component");
var Halogen_HTML = require("../Halogen.HTML");
var Halogen_HTML_Core = require("../Halogen.HTML.Core");
var Halogen_HTML_Elements = require("../Halogen.HTML.Elements");
var Halogen_HTML_Events = require("../Halogen.HTML.Events");
var Halogen_HTML_Properties = require("../Halogen.HTML.Properties");
var Halogen_Query_HalogenM = require("../Halogen.Query.HalogenM");
var LimitQueue = require("../LimitQueue");
var Partial_Unsafe = require("../Partial.Unsafe");
var Prelude = require("../Prelude");
var Cons = (function () {
    function Cons(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Cons.create = function (value0) {
        return function (value1) {
            return new Cons(value0, value1);
        };
    };
    return Cons;
})();
var Unsnoc = (function () {
    function Unsnoc(value0) {
        this.value0 = value0;
    };
    Unsnoc.create = function (value0) {
        return new Unsnoc(value0);
    };
    return Unsnoc;
})();
var ChildMessage = (function () {
    function ChildMessage(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    ChildMessage.create = function (value0) {
        return function (value1) {
            return new ChildMessage(value0, value1);
        };
    };
    return ChildMessage;
})();
var ui_queue = (function () {
    var render = function (state) {
        return Halogen_HTML_Elements.span([ Halogen_HTML_Properties.class_("queue-ui") ])(LimitQueue.toArray(state.queue));
    };
    var initial = function (input) {
        return {
            queue: LimitQueue.empty()(input.limit),
            component: input.component,
            acc: 0
        };
    };
    var $$eval = function (v) {
        if (v instanceof Cons) {
            return Control_Bind.bind(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.get(Halogen_Query_HalogenM.monadStateHalogenM))(function (v1) {
                var child = Halogen_HTML.slot(v1.acc)(v1.component)(v.value0)(Halogen_HTML_Events.input(ChildMessage.create));
                return Control_Bind.discard(Control_Bind.discardUnit)(Halogen_Query_HalogenM.bindHalogenM)(Control_Monad_State_Class.put(Halogen_Query_HalogenM.monadStateHalogenM)((function () {
                    var $6 = {};
                    for (var $7 in v1) {
                        if ({}.hasOwnProperty.call(v1, $7)) {
                            $6[$7] = v1[$7];
                        };
                    };
                    $6.queue = LimitQueue.insert(child)(v1.queue);
                    $6.acc = v1.acc + 1 | 0;
                    return $6;
                })()))(function () {
                    return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(v.value1);
                });
            });
        };
        if (v instanceof Unsnoc) {
            return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(v.value0);
        };
        if (v instanceof ChildMessage) {
            return Control_Applicative.pure(Halogen_Query_HalogenM.applicativeHalogenM)(v.value1);
        };
        throw new Error("Failed pattern match at UI.Queue line 59, column 10 - line 66, column 35: " + [ v.constructor.name ]);
    };
    return Halogen_Component.parentComponent(Data_Ord.ordInt)({
        initialState: initial,
        render: render,
        "eval": $$eval,
        receiver: Data_Function["const"](Data_Maybe.Nothing.value)
    });
})();
module.exports = {
    Cons: Cons,
    Unsnoc: Unsnoc,
    ChildMessage: ChildMessage,
    ui_queue: ui_queue
};