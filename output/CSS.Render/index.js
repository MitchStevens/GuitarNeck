// Generated by purs version 0.11.7
"use strict";
var CSS_Property = require("../CSS.Property");
var CSS_Selector = require("../CSS.Selector");
var CSS_String = require("../CSS.String");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var Control_Alternative = require("../Control.Alternative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Console = require("../Control.Monad.Eff.Console");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Either = require("../Data.Either");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Generic = require("../Data.Generic");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Maybe = require("../Data.Maybe");
var Data_Monoid = require("../Data.Monoid");
var Data_NonEmpty = require("../Data.NonEmpty");
var Data_Ord = require("../Data.Ord");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_These = require("../Data.These");
var Data_Tuple = require("../Data.Tuple");
var Data_Unit = require("../Data.Unit");
var Prelude = require("../Prelude");
var Sheet = function (x) {
    return x;
};
var Inline = function (x) {
    return x;
};
var sepWith = function (s) {
    return function (a) {
        return function (b) {
            return a + (s + b);
        };
    };
};
var semigroupInline = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return v + v1;
    };
});
var semigroupFile = new Data_Semigroup.Semigroup(function (v) {
    return function (v1) {
        return v + v1;
    };
});
var properties = function (xs) {
    var sheetRules = Data_Either.either(function (v) {
        return Data_Monoid.mempty(Data_Monoid.monoidString);
    })(function (v) {
        return Data_Foldable.fold(Data_Foldable.foldableArray)(Data_Monoid.monoidString)([ v.value0, ": ", v.value1 ]);
    });
    return Data_Foldable.intercalate(Data_Foldable.foldableArray)(Data_Monoid.monoidString)("; ")(Data_Functor.map(Data_Functor.functorArray)(sheetRules)(xs));
};
var predicate = function (v) {
    if (v instanceof CSS_Selector.Id) {
        return "#" + v.value0;
    };
    if (v instanceof CSS_Selector.Class) {
        return "." + v.value0;
    };
    if (v instanceof CSS_Selector.Attr) {
        return "[" + (v.value0 + "]");
    };
    if (v instanceof CSS_Selector.AttrVal) {
        return "[" + (v.value0 + ("='" + (v.value1 + "']")));
    };
    if (v instanceof CSS_Selector.AttrBegins) {
        return "[" + (v.value0 + ("^='" + (v.value1 + "']")));
    };
    if (v instanceof CSS_Selector.AttrEnds) {
        return "[" + (v.value0 + ("$='" + (v.value1 + "']")));
    };
    if (v instanceof CSS_Selector.AttrContains) {
        return "[" + (v.value0 + ("*='" + (v.value1 + "']")));
    };
    if (v instanceof CSS_Selector.AttrSpace) {
        return "[" + (v.value0 + ("~='" + (v.value1 + "']")));
    };
    if (v instanceof CSS_Selector.AttrHyph) {
        return "[" + (v.value0 + ("|='" + (v.value1 + "']")));
    };
    if (v instanceof CSS_Selector.Pseudo) {
        return ":" + v.value0;
    };
    if (v instanceof CSS_Selector.PseudoFunc) {
        return ":" + (v.value0 + ("(" + (Data_Foldable.intercalate(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(",")(v.value1) + ")")));
    };
    throw new Error("Failed pattern match at CSS.Render line 180, column 1 - line 180, column 33: " + [ v.constructor.name ]);
};
var selector$prime$prime = function (v) {
    return function (v1) {
        if (v.length === 0 && v1 instanceof CSS_Selector.Star) {
            return [ "*" ];
        };
        if (v1 instanceof CSS_Selector.Star) {
            return [ "" ];
        };
        if (v1 instanceof CSS_Selector.Elem) {
            return [ v1.value0 ];
        };
        if (v1 instanceof CSS_Selector.PathChild) {
            return Control_Apply.apply(Control_Apply.applyArray)(Data_Functor.map(Data_Functor.functorArray)(sepWith(" > "))(selector$prime(v1.value0)))(selector$prime(v1.value1));
        };
        if (v1 instanceof CSS_Selector.Deep) {
            return Control_Apply.apply(Control_Apply.applyArray)(Data_Functor.map(Data_Functor.functorArray)(sepWith(" "))(selector$prime(v1.value0)))(selector$prime(v1.value1));
        };
        if (v1 instanceof CSS_Selector.Adjacent) {
            return Control_Apply.apply(Control_Apply.applyArray)(Data_Functor.map(Data_Functor.functorArray)(sepWith(" + "))(selector$prime(v1.value0)))(selector$prime(v1.value1));
        };
        if (v1 instanceof CSS_Selector.Combined) {
            return Data_Semigroup.append(Data_Semigroup.semigroupArray)(selector$prime(v1.value0))(selector$prime(v1.value1));
        };
        throw new Error("Failed pattern match at CSS.Render line 146, column 1 - line 146, column 63: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var selector$prime = function (v) {
    return Data_Functor.map(Data_Functor.functorArray)(function (v1) {
        return v1 + Data_Foldable.foldMap(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(predicate)(Data_Array.sort(CSS_Selector.ordPredicate)(v.value0));
    })(selector$prime$prime(v.value0)(v.value1));
};
var selector = function ($185) {
    return Data_Foldable.intercalate(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(", ")(selector$prime($185));
};
var nel = function (v) {
    if (v.length === 0) {
        return Data_Maybe.Nothing.value;
    };
    return Data_Functor.map(Data_Maybe.functorMaybe)(function (v1) {
        return new Data_NonEmpty.NonEmpty(v1.head, v1.tail);
    })(Data_Array.uncons(v));
};
var monoidInline = new Data_Monoid.Monoid(function () {
    return semigroupInline;
}, Data_Monoid.mempty(Data_Monoid.monoidString));
var monoidFile = new Data_Monoid.Monoid(function () {
    return semigroupFile;
}, Data_Monoid.mempty(Data_Monoid.monoidString));
var merger = function (v) {
    if (v.value0 instanceof CSS_Stylesheet.Child) {
        return Data_Maybe.maybe(v.value0.value0)(function (xs$prime) {
            return CSS_Selector.child(merger(xs$prime))(v.value0.value0);
        })(nel(v.value1));
    };
    if (v.value0 instanceof CSS_Stylesheet.Sub) {
        return Data_Maybe.maybe(v.value0.value0)(function (xs$prime) {
            return CSS_Selector.deep(merger(xs$prime))(v.value0.value0);
        })(nel(v.value1));
    };
    if (v.value0 instanceof CSS_Stylesheet.Root) {
        return Data_Maybe.maybe(v.value0.value0)(function (xs$prime) {
            return CSS_Selector.deep(v.value0.value0)(merger(xs$prime));
        })(nel(v.value1));
    };
    if (v.value0 instanceof CSS_Stylesheet.Pop) {
        return Data_Maybe.maybe(CSS_Selector.element("TODO"))(merger)(nel(Data_Array.drop(v.value0.value0)(Data_Array.cons(v.value0)(v.value1))));
    };
    if (v.value0 instanceof CSS_Stylesheet.Self) {
        return Data_Maybe.maybe(CSS_Selector["with"](CSS_Selector.star)(v.value0.value0))(function (xs$prime) {
            return CSS_Selector["with"](merger(xs$prime))(v.value0.value0);
        })(nel(v.value1));
    };
    throw new Error("Failed pattern match at CSS.Render line 173, column 3 - line 178, column 106: " + [ v.value0.constructor.name ]);
};
var mediaType = function (v) {
    return CSS_Property.plain(v);
};
var imp = function (t) {
    return Data_Maybe.Just.create(Data_These.That.create(Sheet(CSS_String.fromString(CSS_String.isStringString)("@import url(" + (t + ");\x0a")))));
};
var getSheet = function (v) {
    return v;
};
var renderedSheet = function (v) {
    return Control_Bind.bind(Data_Maybe.bindMaybe)(v)(function ($186) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(getSheet)(Data_These.theseRight($186));
    });
};
var getInline = function (v) {
    return v;
};
var renderedInline = function (v) {
    return Control_Bind.bind(Data_Maybe.bindMaybe)(v)(function ($187) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(getInline)(Data_These.theseLeft($187));
    });
};
var genericSheet = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Render.Sheet" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Sheet))(Data_Generic.fromSpine(Data_Generic.genericString)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Render.Sheet", [ {
        sigConstructor: "CSS.Render.Sheet",
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Render.Sheet", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericString)(v);
    } ]);
});
var genericInline = new Data_Generic.Generic(function (v) {
    if (v instanceof Data_Generic.SProd && (v.value0 === "CSS.Render.Inline" && v.value1.length === 1)) {
        return Control_Apply.apply(Data_Maybe.applyMaybe)(new Data_Maybe.Just(Inline))(Data_Generic.fromSpine(Data_Generic.genericString)(v["value1"][0](Data_Unit.unit)));
    };
    return Data_Maybe.Nothing.value;
}, function ($dollarq) {
    return new Data_Generic.SigProd("CSS.Render.Inline", [ {
        sigConstructor: "CSS.Render.Inline",
        sigValues: [ function ($dollarq1) {
            return Data_Generic.toSignature(Data_Generic.genericString)(Data_Generic.anyProxy);
        } ]
    } ]);
}, function (v) {
    return new Data_Generic.SProd("CSS.Render.Inline", [ function ($dollarq) {
        return Data_Generic.toSpine(Data_Generic.genericString)(v);
    } ]);
});
var feature = function (v) {
    return Data_Maybe.maybe(v.value0)(function (v1) {
        return "(" + (v.value0 + (": " + (CSS_Property.plain(v1) + ")")));
    })(v.value1);
};
var mediaQuery = function (v) {
    return "@media " + (mediaType(v.value1) + Data_NonEmpty.foldl1(Data_Foldable.foldableArray)(Data_Semigroup.append(Data_Semigroup.semigroupString))(Data_Functor.map(Data_NonEmpty.functorNonEmpty(Data_Functor.functorArray))(function ($188) {
        return (function (v1) {
            return " and " + v1;
        })(feature($188));
    })(v.value2)));
};
var eqSheet = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x === y;
    };
});
var ordSheet = new Data_Ord.Ord(function () {
    return eqSheet;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(Data_Ord.ordString)(x)(y);
    };
});
var eqInline = new Data_Eq.Eq(function (x) {
    return function (y) {
        return x === y;
    };
});
var ordInline = new Data_Ord.Ord(function () {
    return eqInline;
}, function (x) {
    return function (y) {
        return Data_Ord.compare(Data_Ord.ordString)(x)(y);
    };
});
var collect$prime = function (v) {
    return function (v1) {
        if (v instanceof CSS_Property.Plain && v1 instanceof CSS_Property.Plain) {
            return [ new Data_Either.Right(new Data_Tuple.Tuple(v.value0, v1.value0)) ];
        };
        if (v instanceof CSS_Property.Prefixed && v1 instanceof CSS_Property.Plain) {
            return Data_Functor.map(Data_Functor.functorArray)(function (v3) {
                return Data_Either.Right.create(new Data_Tuple.Tuple(v3.value0 + v3.value1, v1.value0));
            })(v.value0);
        };
        if (v instanceof CSS_Property.Plain && v1 instanceof CSS_Property.Prefixed) {
            return Data_Functor.map(Data_Functor.functorArray)(function (v2) {
                return Data_Either.Right.create(new Data_Tuple.Tuple(v.value0, v2.value0 + v2.value1));
            })(v1.value0);
        };
        if (v instanceof CSS_Property.Prefixed && v1 instanceof CSS_Property.Prefixed) {
            return Data_Functor.map(Data_Functor.functorArray)(function (v2) {
                return Data_Maybe.maybe(new Data_Either.Left(v2.value0 + v2.value1))(function ($189) {
                    return Data_Either.Right.create(Data_Tuple.Tuple.create(v2.value0 + v2.value1)((function (v3) {
                        return v2.value0 + v3;
                    })($189)));
                })(Data_Tuple.lookup(Data_Foldable.foldableArray)(Data_Eq.eqString)(v2.value0)(v1.value0));
            })(v.value0);
        };
        throw new Error("Failed pattern match at CSS.Render line 161, column 1 - line 161, column 80: " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var collect = function (v) {
    return collect$prime(v.value0)(v.value1);
};
var rule$prime = function (sel) {
    return function (props) {
        var p = Control_Bind.bind(Control_Bind.bindArray)(props)(collect);
        var q = Data_Functor.map(Data_Maybe.functorMaybe)(function ($190) {
            return Data_These.This.create(Inline(properties(Data_NonEmpty.oneOf(Control_Alternative.alternativeArray)($190))));
        })(nel(p));
        var o = function (sel$prime) {
            return Data_Maybe.Just.create(Data_These.That.create(Sheet(Data_Foldable.intercalate(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(" ")([ selector(merger(sel$prime)), "{", properties(p), "}\x0a" ]))));
        };
        return Data_Maybe.maybe(q)(o)(nel(sel));
    };
};
var rules = function (sel) {
    return function (rs) {
        var queries = function (v) {
            if (v instanceof CSS_Stylesheet.Query) {
                return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0, v.value1));
            };
            return Data_Maybe.Nothing.value;
        };
        var queryRules = Data_Foldable.foldMap(Data_Foldable.foldableArray)(Data_Maybe.monoidMaybe(Data_These.semigroupThese(semigroupInline)(semigroupFile)))(Data_Tuple.uncurry(Data_Function.flip(query$prime)(sel)))(Data_Array.mapMaybe(queries)(rs));
        var property = function (v) {
            if (v instanceof CSS_Stylesheet.Property) {
                return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0, v.value1));
            };
            return Data_Maybe.Nothing.value;
        };
        var topRules = (function () {
            var rs$prime = Data_Array.mapMaybe(property)(rs);
            var $172 = Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraFunction(Data_HeytingAlgebra.heytingAlgebraBoolean))(Data_Array["null"])(rs$prime);
            if ($172) {
                return rule$prime(sel)(rs$prime);
            };
            return Data_Maybe.Nothing.value;
        })();
        var nestedRules = function (a) {
            return rules(Data_Array.cons(a)(sel));
        };
        var nested = function (v) {
            if (v instanceof CSS_Stylesheet.Nested) {
                return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0, v.value1));
            };
            return Data_Maybe.Nothing.value;
        };
        var nestedSheets = Data_Foldable.fold(Data_Foldable.foldableArray)(Data_Maybe.monoidMaybe(Data_These.semigroupThese(semigroupInline)(semigroupFile)))(Data_Functor.map(Data_Functor.functorArray)(Data_Tuple.uncurry(nestedRules))(Data_Array.mapMaybe(nested)(rs)));
        var kframes = function (v) {
            if (v instanceof CSS_Stylesheet.Keyframe) {
                return new Data_Maybe.Just(v.value0);
            };
            return Data_Maybe.Nothing.value;
        };
        var keyframeRules = Data_Foldable.foldMap(Data_Foldable.foldableArray)(Data_Maybe.monoidMaybe(Data_These.semigroupThese(semigroupInline)(semigroupFile)))(kframe)(Data_Array.mapMaybe(kframes)(rs));
        var imports = function (v) {
            if (v instanceof CSS_Stylesheet.Import) {
                return new Data_Maybe.Just(v.value0);
            };
            return Data_Maybe.Nothing.value;
        };
        var importRules = Data_Foldable.foldMap(Data_Foldable.foldableArray)(Data_Maybe.monoidMaybe(Data_These.semigroupThese(semigroupInline)(semigroupFile)))(imp)(Data_Array.mapMaybe(imports)(rs));
        var faces = function (v) {
            if (v instanceof CSS_Stylesheet.Face) {
                return new Data_Maybe.Just(v.value0);
            };
            return Data_Maybe.Nothing.value;
        };
        var faceRules = Data_Foldable.foldMap(Data_Foldable.foldableArray)(Data_Maybe.monoidMaybe(Data_These.semigroupThese(semigroupInline)(semigroupFile)))(face)(Data_Array.mapMaybe(faces)(rs));
        return Data_Semigroup.append(Data_Maybe.semigroupMaybe(Data_These.semigroupThese(semigroupInline)(semigroupFile)))(topRules)(Data_Semigroup.append(Data_Maybe.semigroupMaybe(Data_These.semigroupThese(semigroupInline)(semigroupFile)))(importRules)(Data_Semigroup.append(Data_Maybe.semigroupMaybe(Data_These.semigroupThese(semigroupInline)(semigroupFile)))(keyframeRules)(Data_Semigroup.append(Data_Maybe.semigroupMaybe(Data_These.semigroupThese(semigroupInline)(semigroupFile)))(faceRules)(Data_Semigroup.append(Data_Maybe.semigroupMaybe(Data_These.semigroupThese(semigroupInline)(semigroupFile)))(nestedSheets)(queryRules)))));
    };
};
var query$prime = function (q) {
    return function (sel) {
        return function (rs) {
            return Data_Maybe.Just.create(Data_These.That.create(Sheet(mediaQuery(q) + (" { " + (Data_Maybe.fromMaybe("")(renderedSheet(rules(sel)(rs))) + " }\x0a")))));
        };
    };
};
var kframe = function (v) {
    var renderContent = " " + (v.value0 + (" { " + (Data_Foldable.intercalate(Data_NonEmpty.foldableNonEmpty(Data_Foldable.foldableArray))(Data_Monoid.monoidString)(" ")(Data_Functor.map(Data_NonEmpty.functorNonEmpty(Data_Functor.functorArray))(Data_Tuple.uncurry(frame))(v.value1)) + " }\x0a")));
    var keywords = [ "@keyframes", "@-webkit-keyframes", "@-moz-keyframes", "@-o-keyframes" ];
    var allKeywordsWithContent = Data_Foldable.fold(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(Data_Functor.map(Data_Functor.functorArray)(function (v1) {
        return v1 + renderContent;
    })(keywords));
    return Data_Maybe.Just.create(Data_These.That.create(Sheet(allKeywordsWithContent)));
};
var frame = function (p) {
    return function (rs) {
        var x = Data_Maybe.fromMaybe("")(renderedInline(rules([  ])(rs)));
        return Data_Show.show(Data_Show.showNumber)(p) + ("% " + ("{ " + (x + " }")));
    };
};
var face = function (rs) {
    return Data_Maybe.Just.create(Data_These.That.create(Sheet("@font-face { " + (Data_Maybe.fromMaybe("")(renderedInline(rules([  ])(rs))) + " }\x0a"))));
};
var render = function ($191) {
    return rules([  ])(CSS_Stylesheet.runS($191));
};
var putInline = function (s) {
    return Control_Monad_Eff_Console.log(Data_Maybe.fromMaybe("")(renderedInline(render(s))));
};
var putStyleSheet = function (s) {
    return Control_Monad_Eff_Console.log(Data_Maybe.fromMaybe("")(renderedSheet(render(s))));
};
module.exports = {
    Inline: Inline,
    getInline: getInline,
    Sheet: Sheet,
    getSheet: getSheet,
    renderedInline: renderedInline,
    renderedSheet: renderedSheet,
    render: render,
    putInline: putInline,
    putStyleSheet: putStyleSheet,
    kframe: kframe,
    frame: frame,
    "query'": query$prime,
    mediaQuery: mediaQuery,
    mediaType: mediaType,
    feature: feature,
    face: face,
    rules: rules,
    imp: imp,
    "rule'": rule$prime,
    selector: selector,
    "selector'": selector$prime,
    "selector''": selector$prime$prime,
    sepWith: sepWith,
    collect: collect,
    "collect'": collect$prime,
    properties: properties,
    merger: merger,
    predicate: predicate,
    nel: nel,
    eqInline: eqInline,
    ordInline: ordInline,
    genericInline: genericInline,
    semigroupInline: semigroupInline,
    monoidInline: monoidInline,
    eqSheet: eqSheet,
    ordSheet: ordSheet,
    genericSheet: genericSheet,
    semigroupFile: semigroupFile,
    monoidFile: monoidFile
};
