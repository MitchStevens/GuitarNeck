"use strict";
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var Data_Show = require("../Data.Show");
var Music_Transpose = require("../Music.Transpose");
var Prelude = require("../Prelude");
var Fret = function (x) {
    return x;
};
var unfret = function (v) {
    return v;
};
var transpose_fret = new Music_Transpose.Transpose(function (n) {
    return function (v) {
        return Fret(Music_Transpose.mod12(n + v | 0));
    };
});
var show_fret = new Data_Show.Show(function (v) {
    return Data_Show.show(Data_Show.showInt)(v);
});
var semiring_fret = new Data_Semiring.Semiring(function (v) {
    return function (v1) {
        return v + v1 | 0;
    };
}, function (v) {
    return function (v1) {
        return v * v1 | 0;
    };
}, 1, 0);
var ring_fret = new Data_Ring.Ring(function () {
    return semiring_fret;
}, function (v) {
    return function (v1) {
        return v - v1 | 0;
    };
});
var eq_fret = new Data_Eq.Eq(function (v) {
    return function (v1) {
        return v === v1;
    };
});
var ord_fret = new Data_Ord.Ord(function () {
    return eq_fret;
}, Data_Ord.comparing(Data_Ord.ordInt)(function (v) {
    return v;
}));
module.exports = {
    Fret: Fret,
    unfret: unfret,
    eq_fret: eq_fret,
    ord_fret: ord_fret,
    show_fret: show_fret,
    transpose_fret: transpose_fret,
    semiring_fret: semiring_fret,
    ring_fret: ring_fret
};
