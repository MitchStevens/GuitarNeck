// Generated by purs version 0.11.7
"use strict";
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Prelude = require("../Prelude");
var JoinSemilattice = function (join) {
    this.join = join;
};
var MeetSemilattice = function (meet) {
    this.meet = meet;
};
var BoundedJoinSemilattice = function (JoinSemilattice0, bottom) {
    this.JoinSemilattice0 = JoinSemilattice0;
    this.bottom = bottom;
};
var BoundedMeetSemilattice = function (MeetSemilattice0, top) {
    this.MeetSemilattice0 = MeetSemilattice0;
    this.top = top;
};
var Lattice = function (JoinSemilattice0, MeetSemilattice1) {
    this.JoinSemilattice0 = JoinSemilattice0;
    this.MeetSemilattice1 = MeetSemilattice1;
};
var BoundedLattice = function (BoundedJoinSemilattice0, BoundedMeetSemilattice1) {
    this.BoundedJoinSemilattice0 = BoundedJoinSemilattice0;
    this.BoundedMeetSemilattice1 = BoundedMeetSemilattice1;
};
var top = function (dict) {
    return dict.top;
};
var meetSemilatticeBoolean = new MeetSemilattice(Data_HeytingAlgebra.conj(Data_HeytingAlgebra.heytingAlgebraBoolean));
var meet = function (dict) {
    return dict.meet;
};
var joinSemilatticeBoolean = new JoinSemilattice(Data_HeytingAlgebra.disj(Data_HeytingAlgebra.heytingAlgebraBoolean));
var latticeBoolean = new Lattice(function () {
    return joinSemilatticeBoolean;
}, function () {
    return meetSemilatticeBoolean;
});
var join = function (dict) {
    return dict.join;
};
var boundedMeetSemilatticeBoolean = new BoundedMeetSemilattice(function () {
    return meetSemilatticeBoolean;
}, true);
var boundedJoinSemilatticeBoolean = new BoundedJoinSemilattice(function () {
    return joinSemilatticeBoolean;
}, false);
var boundedLatticeBoolean = new BoundedLattice(function () {
    return boundedJoinSemilatticeBoolean;
}, function () {
    return boundedMeetSemilatticeBoolean;
});
var bottom = function (dict) {
    return dict.bottom;
};
module.exports = {
    bottom: bottom,
    join: join,
    meet: meet,
    top: top,
    JoinSemilattice: JoinSemilattice,
    MeetSemilattice: MeetSemilattice,
    BoundedJoinSemilattice: BoundedJoinSemilattice,
    BoundedMeetSemilattice: BoundedMeetSemilattice,
    Lattice: Lattice,
    BoundedLattice: BoundedLattice,
    joinSemilatticeBoolean: joinSemilatticeBoolean,
    meetSemilatticeBoolean: meetSemilatticeBoolean,
    boundedJoinSemilatticeBoolean: boundedJoinSemilatticeBoolean,
    boundedMeetSemilatticeBoolean: boundedMeetSemilatticeBoolean,
    latticeBoolean: latticeBoolean,
    boundedLatticeBoolean: boundedLatticeBoolean
};