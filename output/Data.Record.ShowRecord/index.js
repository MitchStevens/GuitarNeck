// Generated by purs version 0.11.7
"use strict";
var Data_Foldable = require("../Data.Foldable");
var Data_List_Lazy = require("../Data.List.Lazy");
var Data_List_Lazy_Types = require("../Data.List.Lazy.Types");
var Data_Monoid = require("../Data.Monoid");
var Data_Record = require("../Data.Record");
var Data_Semigroup = require("../Data.Semigroup");
var Data_Show = require("../Data.Show");
var Data_Symbol = require("../Data.Symbol");
var Prelude = require("../Prelude");
var Type_Prelude = require("../Type.Prelude");
var Type_Row = require("../Type.Row");
var ShowRowList = function (showRowList) {
    this.showRowList = showRowList;
};
var showRowListNil = new ShowRowList(function (v) {
    return function (v1) {
        return Data_List_Lazy_Types.nil;
    };
});
var showRowList = function (dict) {
    return dict.showRowList;
};
var showRowListConsShow = function (dictShow) {
    return function (dictShowRowList) {
        return function (dictRowCons) {
            return function (dictRowLacks) {
                return function (dictRowToList) {
                    return function (dictIsSymbol) {
                        return new ShowRowList(function (v) {
                            return function (rec) {
                                var rest = showRowList(dictShowRowList)(Type_Row.RLProxy.value)(Data_Record["delete"](dictIsSymbol)(dictRowLacks)(dictRowCons)(Data_Symbol.SProxy.value)(rec));
                                var val = Data_Record.get(dictIsSymbol)(dictRowCons)(Data_Symbol.SProxy.value)(rec);
                                return Data_List_Lazy_Types.cons(Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value) + (": " + Data_Show.show(dictShow)(val)))(rest);
                            };
                        });
                    };
                };
            };
        };
    };
};
var showRecord = function (dictRowToList) {
    return function (dictShowRowList) {
        return function (rec) {
            var rowListStrs = showRowList(dictShowRowList)(Type_Row.RLProxy.value)(rec);
            return "{ " + (Data_Foldable.intercalate(Data_List_Lazy_Types.foldableList)(Data_Monoid.monoidString)(", ")(rowListStrs) + " }");
        };
    };
};
var showRowListConsRecord = function (dictRowToList) {
    return function (dictShowRowList) {
        return function (dictShowRowList1) {
            return function (dictRowCons) {
                return function (dictRowLacks) {
                    return function (dictRowToList1) {
                        return function (dictIsSymbol) {
                            return new ShowRowList(function (v) {
                                return function (rec) {
                                    var rest = showRowList(dictShowRowList1)(Type_Row.RLProxy.value)(Data_Record["delete"](dictIsSymbol)(dictRowLacks)(dictRowCons)(Data_Symbol.SProxy.value)(rec));
                                    var val = Data_Record.get(dictIsSymbol)(dictRowCons)(Data_Symbol.SProxy.value)(rec);
                                    return Data_List_Lazy_Types.cons(Data_Symbol.reflectSymbol(dictIsSymbol)(Data_Symbol.SProxy.value) + (": " + showRecord(dictRowToList)(dictShowRowList)(val)))(rest);
                                };
                            });
                        };
                    };
                };
            };
        };
    };
};
module.exports = {
    ShowRowList: ShowRowList,
    showRowList: showRowList,
    showRecord: showRecord,
    showRowListNil: showRowListNil,
    showRowListConsRecord: showRowListConsRecord,
    showRowListConsShow: showRowListConsShow
};