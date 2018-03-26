// Generated by purs version 0.11.7
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Applicative_Free = require("../Control.Applicative.Free");
var Control_Bind = require("../Control.Bind");
var Control_Coroutine = require("../Control.Coroutine");
var Control_Monad_Aff = require("../Control.Monad.Aff");
var Control_Monad_Aff_Unsafe = require("../Control.Monad.Aff.Unsafe");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Class = require("../Control.Monad.Eff.Class");
var Control_Monad_Eff_Exception = require("../Control.Monad.Eff.Exception");
var Control_Monad_Eff_Ref = require("../Control.Monad.Eff.Ref");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class");
var Control_Monad_Fork_Class = require("../Control.Monad.Fork.Class");
var Control_Monad_Free = require("../Control.Monad.Free");
var Control_Monad_Free_Trans = require("../Control.Monad.Free.Trans");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class");
var Control_Parallel = require("../Control.Parallel");
var Control_Parallel_Class = require("../Control.Parallel.Class");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Boolean = require("../Data.Boolean");
var Data_Coyoneda = require("../Data.Coyoneda");
var Data_Eq = require("../Data.Eq");
var Data_Foldable = require("../Data.Foldable");
var Data_Function = require("../Data.Function");
var Data_Functor = require("../Data.Functor");
var Data_List = require("../Data.List");
var Data_List_Types = require("../Data.List.Types");
var Data_Map = require("../Data.Map");
var Data_Maybe = require("../Data.Maybe");
var Data_Ord = require("../Data.Ord");
var Data_Semiring = require("../Data.Semiring");
var Data_StrMap = require("../Data.StrMap");
var Data_Tuple = require("../Data.Tuple");
var Halogen_Aff_Driver_State = require("../Halogen.Aff.Driver.State");
var Halogen_Aff_Effects = require("../Halogen.Aff.Effects");
var Halogen_Data_OrdBox = require("../Halogen.Data.OrdBox");
var Halogen_Query_EventSource = require("../Halogen.Query.EventSource");
var Halogen_Query_ForkF = require("../Halogen.Query.ForkF");
var Halogen_Query_HalogenM = require("../Halogen.Query.HalogenM");
var Halogen_Query_InputF = require("../Halogen.Query.InputF");
var Prelude = require("../Prelude");
var Unsafe_Reference = require("../Unsafe.Reference");
var queuingHandler = function (handler) {
    return function (ref) {
        return function (message) {
            return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.readRef(ref)))(function (v) {
                if (v instanceof Data_Maybe.Nothing) {
                    return handler(message);
                };
                if (v instanceof Data_Maybe.Just) {
                    return Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.writeRef(ref)(new Data_Maybe.Just(new Data_List_Types.Cons(handler(message), v.value0))));
                };
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval line 174, column 3 - line 178, column 57: " + [ v.constructor.name ]);
            });
        };
    };
};
var handleLifecycle = function (lchs) {
    return function (f) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.writeRef(lchs)({
            initializers: Data_List_Types.Nil.value,
            finalizers: Data_List_Types.Nil.value
        })))(function () {
            return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(f))(function (v) {
                return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.readRef(lchs)))(function (v1) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(Data_Foldable.traverse_(Control_Monad_Aff.applicativeAff)(Data_List_Types.foldableList)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff))(v1.finalizers))(function () {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(Control_Parallel.parSequence_(Control_Monad_Aff.parallelAff)(Data_List_Types.foldableList)(v1.initializers))(function () {
                            return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(v);
                        });
                    });
                });
            });
        });
    };
};
var $$eval = function (render) {
    return function (r) {
        var go = function (ref) {
            return function (v) {
                if (v instanceof Halogen_Query_HalogenM.State) {
                    return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.readRef(ref)))(function (v1) {
                        var v2 = v.value0(v1.state);
                        if (Unsafe_Reference.unsafeRefEq(v1.state)(v2.value1)) {
                            return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(v2.value0);
                        };
                        if (Data_Boolean.otherwise) {
                            return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.writeRef(ref)((function () {
                                var $41 = {};
                                for (var $42 in v1) {
                                    if ({}.hasOwnProperty.call(v1, $42)) {
                                        $41[$42] = v1[$42];
                                    };
                                };
                                $41.state = v2.value1;
                                return $41;
                            })())))(function () {
                                return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(handleLifecycle(v1.lifecycleHandlers)(render(v1.lifecycleHandlers)(ref)))(function () {
                                    return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(v2.value0);
                                });
                            });
                        };
                        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval line 78, column 7 - line 84, column 21: " + [ v2.constructor.name ]);
                    });
                };
                if (v instanceof Halogen_Query_HalogenM.Subscribe) {
                    return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.readRef(ref)))(function (v1) {
                        return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff)(Control_Bind.bind(Control_Monad_Aff.bindAff)(Halogen_Query_EventSource.unEventSource(v.value0))(function (v2) {
                            return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref["modifyRef'"](v1.fresh)(function (i) {
                                return {
                                    state: i + 1 | 0,
                                    value: i
                                };
                            })))(function (v3) {
                                var done$prime = Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.readRef(v1.subscriptions)))(function (v4) {
                                    return Control_Applicative.when(Control_Monad_Aff.applicativeAff)(Data_Maybe.maybe(false)(Data_Map.member(Data_Ord.ordInt)(v3))(v4))(Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(v2.done)(function () {
                                        return Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.modifyRef(v1.subscriptions)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Map["delete"](Data_Ord.ordInt)(v3))));
                                    }));
                                });
                                return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.modifyRef(v1.subscriptions)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Map.insert(Data_Ord.ordInt)(v3)(done$prime)))))(function () {
                                    var consumer = Control_Bind.bind(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorAwait)(Control_Monad_Aff.monadAff))(Control_Coroutine["await"](Control_Monad_Aff.monadAff))(function (v4) {
                                        return Control_Bind.bind(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorAwait)(Control_Monad_Aff.monadAff))(Control_Monad_Trans_Class.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorAwait))(Control_Monad_Aff.monadAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.readRef(v1.subscriptions))))(function (v5) {
                                            return Control_Applicative.when(Control_Monad_Free_Trans.applicativeFreeT(Control_Coroutine.functorAwait)(Control_Monad_Aff.monadAff))(Data_Maybe.isJust(v5))(Control_Bind.bind(Control_Monad_Free_Trans.bindFreeT(Control_Coroutine.functorAwait)(Control_Monad_Aff.monadAff))(Control_Monad_Trans_Class.lift(Control_Monad_Free_Trans.monadTransFreeT(Control_Coroutine.functorAwait))(Control_Monad_Aff.monadAff)(evalF(ref)(v4)))(function (v6) {
                                                return Control_Applicative.when(Control_Monad_Free_Trans.applicativeFreeT(Control_Coroutine.functorAwait)(Control_Monad_Aff.monadAff))(Data_Eq.eq(Halogen_Query_EventSource.eqSubscribeStatus)(v6)(Halogen_Query_EventSource.Listening.value))(consumer);
                                            }));
                                        });
                                    });
                                    return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(Control_Coroutine.runProcess(Control_Monad_Aff.monadRecAff)(Control_Coroutine.pullFrom(Control_Monad_Aff.monadRecAff)(consumer)(v2.producer)))(function () {
                                        return done$prime;
                                    });
                                });
                            });
                        })))(function (v2) {
                            return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(v.value1);
                        });
                    });
                };
                if (v instanceof Halogen_Query_HalogenM.Lift) {
                    return v.value0;
                };
                if (v instanceof Halogen_Query_HalogenM.Halt) {
                    return Control_Monad_Error_Class.throwError(Control_Monad_Aff.monadThrowAff)(Control_Monad_Eff_Exception.error(v.value0));
                };
                if (v instanceof Halogen_Query_HalogenM.GetSlots) {
                    return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.readRef(ref)))(function (v1) {
                        return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(v.value0(Data_Functor.map(Data_List_Types.functorList)(Halogen_Data_OrdBox.unOrdBox)(Data_Map.keys(v1.children))));
                    });
                };
                if (v instanceof Halogen_Query_HalogenM.CheckSlot) {
                    return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.readRef(ref)))(function (v1) {
                        return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(v.value1(Data_Map.member(Halogen_Data_OrdBox.ordOrdBox)(v1.component.mkOrdBox(v.value0))(v1.children)));
                    });
                };
                if (v instanceof Halogen_Query_HalogenM.ChildQuery) {
                    return evalChildQuery(ref)(v.value0)(v.value1);
                };
                if (v instanceof Halogen_Query_HalogenM.Raise) {
                    return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.readRef(ref)))(function (v1) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(queuingHandler(v1.handler)(v1.pendingOuts)(v.value0))(function () {
                            return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(v.value1);
                        });
                    });
                };
                if (v instanceof Halogen_Query_HalogenM.Par) {
                    return Control_Parallel_Class.sequential(Control_Monad_Aff.parallelAff)(Control_Applicative_Free.retractFreeAp(Control_Monad_Aff.applicativeParAff)(Control_Applicative_Free.hoistFreeAp(function ($109) {
                        return Control_Parallel_Class.parallel(Control_Monad_Aff.parallelAff)(evalM(ref)($109));
                    })(v.value0)));
                };
                if (v instanceof Halogen_Query_HalogenM.Fork) {
                    return Halogen_Query_ForkF.unFork(function (v1) {
                        return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Fork_Class.fork(Control_Monad_Fork_Class.monadForkAff)(evalM(ref)(v1.value0)))(function (v2) {
                            return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(v1.value1(function ($110) {
                                return Control_Monad_Aff_Unsafe.unsafeCoerceAff(Data_Function.flip(Control_Monad_Aff.killFiber)(v2)($110));
                            }));
                        });
                    })(v.value0);
                };
                if (v instanceof Halogen_Query_HalogenM.GetRef) {
                    return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.readRef(ref)))(function (v1) {
                        return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(v.value1(Data_StrMap.lookup(v.value0)(v1.refs)));
                    });
                };
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval line 75, column 12 - line 131, column 34: " + [ v.constructor.name ]);
            };
        };
        var evalM = function (ref) {
            return function (v) {
                return Control_Monad_Free.foldFree(Control_Monad_Aff.monadRecAff)(go(ref))(v);
            };
        };
        var evalF = function (ref) {
            return function (q) {
                return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.readRef(ref)))(function (v) {
                    var v1 = v["component"]["eval"](q);
                    return Control_Monad_Free.foldFree(Control_Monad_Aff.monadRecAff)(go(ref))(v1);
                });
            };
        };
        var evalChildQuery = function (ref) {
            return function (p) {
                return Data_Coyoneda.unCoyoneda(function (k) {
                    return function (q) {
                        return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.readRef(ref)))(function (v) {
                            var v1 = Data_Map.lookup(Halogen_Data_OrdBox.ordOrdBox)(v.component.mkOrdBox(p))(v.children);
                            if (v1 instanceof Data_Maybe.Just) {
                                return Control_Bind.bind(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.readRef(v1.value0)))(function (v2) {
                                    return Halogen_Aff_Driver_State.unDriverStateX(function (ds) {
                                        var v3 = ds.prjQuery(q);
                                        if (v3 instanceof Data_Maybe.Just) {
                                            return Data_Functor.map(Control_Monad_Aff.functorAff)(k)(evalF(ds.selfRef)(v3.value0));
                                        };
                                        if (v3 instanceof Data_Maybe.Nothing) {
                                            return Control_Monad_Error_Class.throwError(Control_Monad_Aff.monadThrowAff)(Control_Monad_Eff_Exception.error("Query projection failed for child query"));
                                        };
                                        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval line 144, column 32 - line 146, column 82: " + [ v3.constructor.name ]);
                                    })(v2);
                                });
                            };
                            if (v1 instanceof Data_Maybe.Nothing) {
                                return Control_Monad_Error_Class.throwError(Control_Monad_Aff.monadThrowAff)(Control_Monad_Eff_Exception.error("Slot lookup failed for child query"));
                            };
                            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval line 141, column 5 - line 147, column 73: " + [ v1.constructor.name ]);
                        });
                    };
                });
            };
        };
        return function (v) {
            if (v instanceof Halogen_Query_InputF.RefUpdate) {
                return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Aff.bindAff)(Control_Monad_Eff_Class.liftEff(Control_Monad_Aff.monadEffAff)(Control_Monad_Eff_Ref.modifyRef(r)(function (v1) {
                    var $102 = {};
                    for (var $103 in v1) {
                        if ({}.hasOwnProperty.call(v1, $103)) {
                            $102[$103] = v1[$103];
                        };
                    };
                    $102.refs = Data_StrMap.alter(Data_Function["const"](v.value1))(v.value0)(v1.refs);
                    return $102;
                })))(function () {
                    return Control_Applicative.pure(Control_Monad_Aff.applicativeAff)(v.value2);
                });
            };
            if (v instanceof Halogen_Query_InputF.Query) {
                return evalF(r)(v.value0);
            };
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval line 61, column 3 - line 66, column 25: " + [ v.constructor.name ]);
        };
    };
};
module.exports = {
    handleLifecycle: handleLifecycle,
    "eval": $$eval,
    queuingHandler: queuingHandler
};