// Generated by purs version 0.11.7
"use strict";
var Control_Alt = require("../Control.Alt");
var Control_Applicative = require("../Control.Applicative");
var Control_Apply = require("../Control.Apply");
var Control_Bind = require("../Control.Bind");
var Control_Plus = require("../Control.Plus");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Array = require("../Data.Array");
var Data_Char_Unicode = require("../Data.Char.Unicode");
var Data_Either = require("../Data.Either");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Identity = require("../Data.Identity");
var Data_Int = require("../Data.Int");
var Data_List = require("../Data.List");
var Data_List_Types = require("../Data.List.Types");
var Data_Maybe = require("../Data.Maybe");
var Data_Ring = require("../Data.Ring");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Data_String = require("../Data.String");
var Fingering = require("../Fingering");
var Fret = require("../Fret");
var Music = require("../Music");
var Music_Chord = require("../Music.Chord");
var Music_Extension = require("../Music.Extension");
var Music_Mode = require("../Music.Mode");
var Music_Note = require("../Music.Note");
var Prelude = require("../Prelude");
var Text_Parsing_Parser = require("../Text.Parsing.Parser");
var Text_Parsing_Parser_Combinators = require("../Text.Parsing.Parser.Combinators");
var Text_Parsing_Parser_String = require("../Text.Parsing.Parser.String");
var Text_Parsing_Parser_Token = require("../Text.Parsing.Parser.Token");
var parse_mode = (function () {
    var end = Text_Parsing_Parser_Combinators.lookAhead(Data_Identity.monadIdentity)(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Data_Functor["void"](Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String.satisfy(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)(function ($30) {
        return !Data_Char_Unicode.isAlpha($30);
    }))))(Text_Parsing_Parser_String.eof(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)));
    var majP = Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("Maj")))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("maj"))))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("M"))(end))))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("\u0394"))))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Music_Mode.Major.value));
    var minP = Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("m"))(end)))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("min"))))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("Min"))))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("-"))(end))))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Music_Mode.Minor.value));
    var domP = Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("dom"))(end)))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Music_Mode.Mixolydian.value));
    var dimP = Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("o"))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("dim")))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Music_Mode.Diminished.value));
    var augP = Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("+"))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("aug")))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Music_Mode.Augmented.value));
    return Text_Parsing_Parser_Combinators.option(Data_Identity.monadIdentity)(Music_Mode.Mixolydian.value)(Text_Parsing_Parser_Combinators.choice(Data_Foldable.foldableArray)(Data_Identity.monadIdentity)([ majP, minP, domP, augP, dimP ]));
})();
var parse_int = Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_String.fromCharArray)(Data_Array.some(Text_Parsing_Parser.alternativeParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser.lazyParserT)(Text_Parsing_Parser_Token.digit(Data_Identity.monadIdentity))))(function (v) {
    var v1 = Data_Int.fromString(v);
    if (v1 instanceof Data_Maybe.Just) {
        return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(v1.value0);
    };
    if (v1 instanceof Data_Maybe.Nothing) {
        return Text_Parsing_Parser.fail(Data_Identity.monadIdentity)("Couldn't parse " + (v + "."));
    };
    throw new Error("Failed pattern match at Parser line 107, column 3 - line 111, column 1: " + [ v1.constructor.name ]);
});
var parse_fret = Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(function ($31) {
    return Data_Maybe.Just.create(Fret.Fret($31));
})(parse_int))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("x"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Data_Maybe.Nothing.value)));
var parse_fingering = Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Data_Array.fromFoldable(Data_List_Types.foldableList))(Text_Parsing_Parser_Combinators.sepBy(Data_Identity.monadIdentity)(parse_fret)(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("-"))))(function (v) {
    if (v.length === 6) {
        return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))({
            e4: v[5],
            b3: v[4],
            g3: v[3],
            d3: v[2],
            a2: v[1],
            e2: v[0]
        });
    };
    return Text_Parsing_Parser.fail(Data_Identity.monadIdentity)("Couldn't parse " + (Data_Show.show(Data_Show.showArray(Data_Maybe.showMaybe(Fret.show_fret)))(v) + "."));
});
var parse_extension = (function () {
    var parse_ext_num = Data_Foldable.oneOf(Data_Foldable.foldableArray)(Text_Parsing_Parser.plusParserT(Data_Identity.monadIdentity))([ Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("5"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(5)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("6"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(6)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("7"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(7)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("9"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(9)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("11"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(11)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("13"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(13)) ]);
    return Data_Foldable.oneOf(Data_Foldable.foldableArray)(Text_Parsing_Parser.plusParserT(Data_Identity.monadIdentity))([ Data_Functor.mapFlipped(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("b"))(parse_ext_num))(Music_Extension.Flat.create), Data_Functor.mapFlipped(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("#"))(parse_ext_num))(Music_Extension.Sharp.create), Data_Functor.mapFlipped(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(parse_ext_num)(Music_Extension.Add.create) ]);
})();
var parse_head_ext = Data_Foldable.oneOf(Data_Foldable.foldableArray)(Text_Parsing_Parser.plusParserT(Data_Identity.monadIdentity))([ Data_Functor.mapFlipped(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(parse_extension)(Control_Applicative.pure(Control_Applicative.applicativeArray)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("sus2"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))([ Music_Extension.Sus2.value ])), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("sus4"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))([ Music_Extension.Sus4.value ])) ]);
var parse_tail_ext = Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("("))(parse_extension))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)(")"));
var parse_accidental = Data_Foldable.oneOf(Data_Foldable.foldableArray)(Text_Parsing_Parser.plusParserT(Data_Identity.monadIdentity))([ Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("#"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(1)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("b"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(-1 | 0)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("\u266e"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(0)) ]);
var parse_note = (function () {
    var note_num = Data_Foldable.oneOf(Data_Foldable.foldableArray)(Text_Parsing_Parser.plusParserT(Data_Identity.monadIdentity))([ Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("C"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(0)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("D"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(2)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("E"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(4)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("F"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(5)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("G"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(7)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("A"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(9)), Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("B"))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(11)) ]);
    return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(note_num)(function (v) {
        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.option(Data_Identity.monadIdentity)(0)(parse_accidental))(function (v1) {
            return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Music_Note.pitch(v + v1 | 0));
        });
    });
})();
var parse_chord = Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(parse_note)(function (v) {
    return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(parse_mode)(function (v1) {
        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.option(Data_Identity.monadIdentity)([  ])(parse_head_ext))(function (v2) {
            return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Data_Array.many(Text_Parsing_Parser.alternativeParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser.lazyParserT)(parse_tail_ext))(function (v3) {
                return Control_Bind.discard(Control_Bind.discardUnit)(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.eof(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity))(function () {
                    var v4 = Music_Chord.mk_chord(v)(v1)(Data_Semigroup.append(Data_Semigroup.semigroupArray)(v2)(v3));
                    if (v4 instanceof Data_Either.Left) {
                        return Text_Parsing_Parser.fail(Data_Identity.monadIdentity)(Data_Show.show(Data_Show.showString)(v4.value0));
                    };
                    if (v4 instanceof Data_Either.Right) {
                        return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(v4.value0);
                    };
                    throw new Error("Failed pattern match at Parser line 100, column 3 - line 102, column 30: " + [ v4.constructor.name ]);
                });
            });
        });
    });
});
module.exports = {
    parse_chord: parse_chord,
    parse_fingering: parse_fingering
};