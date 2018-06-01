// Generated by purs version 0.11.7
"use strict";
var CSS_Property = require("../CSS.Property");
var CSS_Render = require("../CSS.Render");
var CSS_Stylesheet = require("../CSS.Stylesheet");
var Control_Bind = require("../Control.Bind");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Either = require("../Data.Either");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Maybe = require("../Data.Maybe");
var Data_MediaType = require("../Data.MediaType");
var Data_Monoid = require("../Data.Monoid");
var Data_Semigroup = require("../Data.Semigroup");
var Data_StrMap = require("../Data.StrMap");
var Data_String = require("../Data.String");
var Data_Tuple = require("../Data.Tuple");
var Halogen_HTML = require("../Halogen.HTML");
var Halogen_HTML_Core = require("../Halogen.HTML.Core");
var Halogen_HTML_Elements = require("../Halogen.HTML.Elements");
var Halogen_HTML_Properties = require("../Halogen.HTML.Properties");
var Prelude = require("../Prelude");
var stylesheet = function (css) {
    var content = Data_Maybe.fromMaybe("")(CSS_Render.renderedSheet(CSS_Render.render(css)));
    return Halogen_HTML_Elements.style([ Halogen_HTML_Properties.type_(Halogen_HTML_Core.mediaTypeIsProp)("text/css") ])([ Halogen_HTML_Core.text(content) ]);
};
var style = (function () {
    var toString = function ($4) {
        return Data_String.joinWith("; ")(Data_StrMap.foldMap(Data_Monoid.monoidArray)(function (key) {
            return function (val) {
                return [ key + (": " + val) ];
            };
        })($4));
    };
    var rights = Data_Array.concatMap(Data_Foldable.foldMap(Data_Either.foldableEither)(Data_Monoid.monoidArray)(Data_Array.singleton));
    var property = function (v) {
        if (v instanceof CSS_Stylesheet.Property) {
            return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0, v.value1));
        };
        return Data_Maybe.Nothing.value;
    };
    var rules = function (rs) {
        var properties = Control_Bind.bind(Control_Bind.bindArray)(Data_Array.mapMaybe(property)(rs))(function ($5) {
            return rights(CSS_Render.collect($5));
        });
        return Data_StrMap.fromFoldable(Data_Foldable.foldableArray)(properties);
    };
    return function ($6) {
        return Halogen_HTML_Properties.attr("style")(toString(rules(CSS_Stylesheet.runS($6))));
    };
})();
module.exports = {
    style: style,
    stylesheet: stylesheet
};