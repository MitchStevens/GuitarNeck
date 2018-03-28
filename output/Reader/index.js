// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Aff_AVar = require("../Control.Monad.Aff.AVar");
var Control_Monad_Aff_Class = require("../Control.Monad.Aff.Class");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Eff_Console = require("../Control.Monad.Eff.Console");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Eff_Ref = require("../Control.Monad.Eff.Ref");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DOM_HTML_HTMLFormElement = require("../DOM.HTML.HTMLFormElement");
var Data_Argonaut_Core = require("../Data.Argonaut.Core");
var Data_Argonaut_Parser = require("../Data.Argonaut.Parser");
var Data_Array = require("../Data.Array");
var Data_Either = require("../Data.Either");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_Int = require("../Data.Int");
var Data_Maybe = require("../Data.Maybe");
var Data_Semigroup = require("../Data.Semigroup");
var Data_StrMap = require("../Data.StrMap");
var Data_String = require("../Data.String");
var Data_Traversable = require("../Data.Traversable");
var Debug_Trace = require("../Debug.Trace");
var Fingering = require("../Fingering");
var Fret = require("../Fret");
var JsonParser = require("../JsonParser");
var Music = require("../Music");
var Music_Chord = require("../Music.Chord");
var Music_Note = require("../Music.Note");
var NeckData = require("../NeckData");
var Network_HTTP_Affjax = require("../Network.HTTP.Affjax");
var Network_HTTP_Affjax_Response = require("../Network.HTTP.Affjax.Response");
var Parsing = require("../Parsing");
var Prelude = require("../Prelude");
var read_fingerings = function (origin) {
    var decode = function ($15) {
        return JsonParser.decode_fingering_map((function (x) {
            return x.response;
        })($15));
    };
    var cache = function (open) {
        return function (move) {
            return {
                open: open,
                moveable: move
            };
        };
    };
    return Control_Bind.bind(Control_Monad_Aff.bindAff)(Data_Functor.map(Control_Monad_Aff.functorAff)(decode)(Network_HTTP_Affjax.get(Network_HTTP_Affjax_Response.responsableJson)(origin + "/chords/open")))(function (v) {
        return Control_Bind.bind(Control_Monad_Aff.bindAff)(Data_Functor.map(Control_Monad_Aff.functorAff)(decode)(Network_HTTP_Affjax.get(Network_HTTP_Affjax_Response.responsableJson)(origin + "/chords/moveable")))(function (v1) {
            var v2 = Control_Apply.lift2(Data_Either.applyEither)(cache)(v)(v1);
            if (v2 instanceof Data_Either.Left) {
                return Control_Monad_Error_Class.throwError(Control_Monad_Aff.monadThrowAff)(Control_Monad_Eff_Exception.error(v2.value0));
            };
            if (v2 instanceof Data_Either.Right) {
                return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(v2.value0);
            };
            throw new Error("Failed pattern match at Reader line 57, column 3 - line 59, column 30: " + [ v2.constructor.name ]);
        });
    });
};
var get_fingerings = function (fingerings) {
    return function (v) {
        var suffix = Music_Chord.chord_suffix(v);
        var note_num = Debug_Trace.trace(Debug_Trace.warn())(suffix)(function (v1) {
            return Music_Note.pcToInt(v.value0);
        });
        return Data_Functor.map(Data_Functor.functorArray)(Fingering.shift_fingering(note_num))(Data_Maybe.fromMaybe([  ])(Data_StrMap.lookup(suffix)(fingerings.moveable)));
    };
};
module.exports = {
    read_fingerings: read_fingerings,
    get_fingerings: get_fingerings
};
