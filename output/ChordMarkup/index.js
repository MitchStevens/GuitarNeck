"use strict";
var DOM = require("../DOM");
var DOM_Event_MouseEvent = require("../DOM.Event.MouseEvent");
var DOM_Event_Types = require("../DOM.Event.Types");
var DOM_HTML_HTMLElement = require("../DOM.HTML.HTMLElement");
var DOM_HTML_Types = require("../DOM.HTML.Types");
var DOM_Node_ParentNode = require("../DOM.Node.ParentNode");
var Data_Array = require("../Data.Array");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Show = require("../Data.Show");
var Graphics_Canvas = require("../Graphics.Canvas");
var Halogen = require("../Halogen");
var Halogen_Aff = require("../Halogen.Aff");
var Halogen_HTML = require("../Halogen.HTML");
var Halogen_HTML_Events = require("../Halogen.HTML.Events");
var Halogen_HTML_Properties = require("../Halogen.HTML.Properties");
var Music = require("../Music");
var Music_Chord = require("../Music.Chord");
var Music_Extension = require("../Music.Extension");
var Music_Mode = require("../Music.Mode");
var Music_Note = require("../Music.Note");
var Network_HTTP_Affjax = require("../Network.HTTP.Affjax");
var Node_FS = require("../Node.FS");
var Prelude = require("../Prelude");
var markup = function (v) {
    var mk_exts = function (x) {
        return function (y) {
            return {
                root: Data_Show.show(Music_Note.show_pitchclass)(v.value0),
                qual: Data_Show.show(Music_Mode.show_mode)(v.value1),
                head_ext: x,
                tail_ext: y
            };
        };
    };
    var v1 = Data_Array.uncons(Data_Functor.map(Data_Functor.functorArray)(Data_Show.show(Music_Extension.show_extension))(v.value2));
    if (v1 instanceof Data_Maybe.Just) {
        return mk_exts(v1.value0.head)(v1.value0.tail);
    };
    if (v1 instanceof Data_Maybe.Nothing) {
        return mk_exts("")([  ]);
    };
    throw new Error("Failed pattern match at ChordMarkup line 31, column 33 - line 33, column 44: " + [ v1.constructor.name ]);
};
module.exports = {
    markup: markup
};
