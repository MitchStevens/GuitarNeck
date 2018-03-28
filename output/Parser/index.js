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
    var end = Text_Parsing_Parser_Combinators.lookAhead(Data_Identity.monadIdentity)(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Data_Functor["void"](Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String.satisfy(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)(function ($26) {
        return !Data_Char_Unicode.isAlpha($26);
    }))))(Text_Parsing_Parser_String.eof(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)));
    var majP = Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("Maj")))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("maj"))))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("M"))(end))))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("\u0394"))))(Music_Mode.Major.value);
    var minP = Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("m"))(end)))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("min"))))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("Min"))))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("-"))(end))))(Music_Mode.Minor.value);
    var domP = Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_Combinators["try"](Data_Identity.monadIdentity)(Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("dom"))(end)))(Music_Mode.Mixolydian.value);
    var dimP = Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("o"))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("dim")))(Music_Mode.Diminished.value);
    var augP = Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("+"))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("aug")))(Music_Mode.Augmented.value);
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
    throw new Error("Failed pattern match at Parser line 114, column 3 - line 118, column 1: " + [ v1.constructor.name ]);
});
var parse_fret = Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Data_Functor.map(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(function ($27) {
    return Data_Maybe.Just.create(Fret.Fret($27));
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
    var parse_ext_num = Text_Parsing_Parser_Combinators.choice(Data_Foldable.foldableArray)(Data_Identity.monadIdentity)([ Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("5"))(5), Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("6"))(6), Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("7"))(7), Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("9"))(9), Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("11"))(11), Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("13"))(13), Text_Parsing_Parser.fail(Data_Identity.monadIdentity)("Couldn't parse Extension") ]);
    return Text_Parsing_Parser_Combinators.choice(Data_Foldable.foldableArray)(Data_Identity.monadIdentity)([ Data_Functor.mapFlipped(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("b"))(parse_ext_num))(Music_Extension.Flat.create), Data_Functor.mapFlipped(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("#"))(parse_ext_num))(Music_Extension.Sharp.create), Data_Functor.mapFlipped(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(parse_ext_num)(Music_Extension.Add.create), Text_Parsing_Parser.fail(Data_Identity.monadIdentity)("Couldn't read extension") ]);
})();
var parse_head_ext = Text_Parsing_Parser_Combinators.choice(Data_Foldable.foldableArray)(Data_Identity.monadIdentity)([ Data_Functor.mapFlipped(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(parse_extension)(Control_Applicative.pure(Control_Applicative.applicativeArray)), Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("sus2"))([ Music_Extension.Sus2.value ]), Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String.string(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("sus4"))([ Music_Extension.Sus4.value ]) ]);
var parse_tail_ext = Control_Apply.applyFirst(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("("))(parse_extension))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)(")"));
var parse_accidental = Data_Foldable.oneOf(Data_Foldable.foldableArray)(Text_Parsing_Parser.plusParserT(Data_Identity.monadIdentity))([ Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("#"))(1), Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("b"))(-1 | 0), Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("\u266e"))(0) ]);
var parse_note = (function () {
    var note_num = Data_Foldable.oneOf(Data_Foldable.foldableArray)(Text_Parsing_Parser.plusParserT(Data_Identity.monadIdentity))([ Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("C"))(0), Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("D"))(2), Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("E"))(4), Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("F"))(5), Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("G"))(7), Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("A"))(9), Data_Functor.voidLeft(Text_Parsing_Parser.functorParserT(Data_Identity.functorIdentity))(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity)("B"))(11) ]);
    var err_lower = Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Data_Foldable.oneOf(Data_Foldable.foldableArray)(Text_Parsing_Parser.plusParserT(Data_Identity.monadIdentity))(Data_Functor.map(Data_Functor.functorArray)(Text_Parsing_Parser_String["char"](Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity))(Data_String.toCharArray("abcdefg"))))(Text_Parsing_Parser.fail(Data_Identity.monadIdentity)("Note names must be in uppercase, eg. 'E' not 'e'"));
    var err_empty = Control_Apply.applySecond(Text_Parsing_Parser.applyParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.eof(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity))(Text_Parsing_Parser.fail(Data_Identity.monadIdentity)("Waiting for chord..."));
    var err = Text_Parsing_Parser.fail(Data_Identity.monadIdentity)("Couldn't read root note. Examples of valid root notes: 'A', 'Bb', 'F#'");
    return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.choice(Data_Foldable.foldableArray)(Data_Identity.monadIdentity)([ note_num, err_lower, err, err_empty ]))(function (v) {
        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.option(Data_Identity.monadIdentity)(0)(parse_accidental))(function (v1) {
            return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(Music_Note.pitch(v + v1 | 0));
        });
    });
})();
var parse_chord = Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(parse_note)(function (v) {
    return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(parse_mode)(function (v1) {
        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.option(Data_Identity.monadIdentity)([  ])(parse_head_ext))(function (v2) {
            return Control_Bind.bind(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.withErrorMessage(Data_Identity.monadIdentity)(Data_Array.many(Text_Parsing_Parser.alternativeParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser.lazyParserT)(parse_tail_ext))("^ext error"))(function (v3) {
                return Control_Bind.discard(Control_Bind.discardUnit)(Text_Parsing_Parser.bindParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Combinators.withErrorMessage(Data_Identity.monadIdentity)(Text_Parsing_Parser_String.eof(Text_Parsing_Parser_String.stringLikeString)(Data_Identity.monadIdentity))("Couldn't understand this chord"))(function () {
                    return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(Data_Identity.monadIdentity))(new Music_Chord.Chord(v, v1, Data_Semigroup.append(Data_Semigroup.semigroupArray)(v2)(v3)));
                });
            });
        });
    });
});
module.exports = {
    parse_chord: parse_chord,
    parse_fingering: parse_fingering
};
