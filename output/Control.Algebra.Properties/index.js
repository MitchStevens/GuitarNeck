// Generated by purs version 0.11.7
"use strict";
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Eq = require("../Data.Eq");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Prelude = require("../Prelude");
var zeropotent = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return Data_Eq.eq(dictEq)(op(op(a)(a))(b))(op(a)(op(b)(b))) && Data_Eq.eq(dictEq)(op(op(a)(a))(b))(op(a)(a));
            };
        };
    };
};
var unipotent = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return Data_Eq.eq(dictEq)(op(a)(a))(op(b)(b));
            };
        };
    };
};
var truthPreserving = function (dictHeytingAlgebra) {
    return function (dictEq) {
        return function (op) {
            return function (a) {
                return function (b) {
                    return Data_HeytingAlgebra.implies(dictHeytingAlgebra)(Data_HeytingAlgebra.conj(dictHeytingAlgebra)(a)(b))(op(a)(b));
                };
            };
        };
    };
};
var transitive = function (dictHeytingAlgebra) {
    return function (op) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return Data_HeytingAlgebra.implies(dictHeytingAlgebra)(Data_HeytingAlgebra.conj(dictHeytingAlgebra)(op(a)(b))(op(b)(c)))(op(a)(c));
                };
            };
        };
    };
};
var rightUnar = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return Data_Eq.eq(dictEq)(op(b)(a))(op(c)(a));
                };
            };
        };
    };
};
var rightSemimedial = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return Data_Eq.eq(dictEq)(op(op(b)(c))(op(a)(a)))(op(op(b)(a))(op(c)(a)));
                    };
                };
            };
        };
    };
};
var rightDistributive = function (dictEq) {
    return function (f) {
        return function (g) {
            return function (a) {
                return function (b) {
                    return function (c) {
                        return Data_Eq.eq(dictEq)(g(f(a)(b))(c))(f(g(a)(c))(g(b)(c)));
                    };
                };
            };
        };
    };
};
var rightCancellative = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return Data_HeytingAlgebra.implies(Data_HeytingAlgebra.heytingAlgebraBoolean)(Data_Eq.eq(dictEq)(op(b)(a))(op(c)(a)))(Data_Eq.eq(dictEq)(b)(c));
                };
            };
        };
    };
};
var rightAlternativeIdentity = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return Data_Eq.eq(dictEq)(op(op(b)(a))(a))(op(b)(op(a)(a)));
            };
        };
    };
};
var reflexive = function (dictHeytingAlgebra) {
    return function (dictEq) {
        return function (op) {
            return function (a) {
                return op(a)(a);
            };
        };
    };
};
var powerAssociative = function (dictEq) {
    return function (op) {
        return function (a) {
            return Data_Eq.eq(dictEq)(op(a)(op(a)(a)))(op(op(a)(a))(a));
        };
    };
};
var monotonic = function (dictHeytingAlgebra) {
    return function (dictEq) {
        return function (op) {
            return function (a) {
                return function (b) {
                    return function (c) {
                        return Data_HeytingAlgebra.implies(dictHeytingAlgebra)(Data_HeytingAlgebra.implies(dictHeytingAlgebra)(a)(b))(Data_HeytingAlgebra.implies(dictHeytingAlgebra)(op(a)(c))(op(b)(c)));
                    };
                };
            };
        };
    };
};
var medial = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return Data_Eq.eq(dictEq)(op(op(a)(b))(op(c)(d)))(op(op(a)(c))(op(b)(d)));
                    };
                };
            };
        };
    };
};
var leftUnar = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return Data_Eq.eq(dictEq)(op(a)(b))(op(a)(c));
                };
            };
        };
    };
};
var leftSemimedial = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return Data_Eq.eq(dictEq)(op(op(a)(a))(op(b)(c)))(op(op(a)(b))(op(a)(c)));
                    };
                };
            };
        };
    };
};
var semimedial = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return function (d) {
                        return leftSemimedial(dictEq)(op)(a)(b)(c)(d) && rightSemimedial(dictEq)(op)(a)(b)(c)(d);
                    };
                };
            };
        };
    };
};
var leftDistributive = function (dictEq) {
    return function (f) {
        return function (g) {
            return function (a) {
                return function (b) {
                    return function (c) {
                        return Data_Eq.eq(dictEq)(f(a)(g(b)(c)))(g(f(a)(b))(f(a)(c)));
                    };
                };
            };
        };
    };
};
var leftCancellative = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return Data_HeytingAlgebra.implies(Data_HeytingAlgebra.heytingAlgebraBoolean)(Data_Eq.eq(dictEq)(op(a)(b))(op(a)(c)))(Data_Eq.eq(dictEq)(b)(c));
                };
            };
        };
    };
};
var leftAlternativeIdentity = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return Data_Eq.eq(dictEq)(op(a)(op(a)(b)))(op(op(a)(a))(b));
            };
        };
    };
};
var jordanIdentity = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return Data_Eq.eq(dictEq)(op(op(a)(b))(op(a)(a)))(op(a)(op(b)(op(a)(a))));
            };
        };
    };
};
var jacobiIdentity = function (dictEq) {
    return function (sum) {
        return function (product) {
            return function (additiveId) {
                return function (a) {
                    return function (b) {
                        return function (c) {
                            return Data_Eq.eq(dictEq)(sum(sum(product(a)(product(b)(c)))(product(b)(product(c)(a))))(product(c)(product(a)(b))))(additiveId);
                        };
                    };
                };
            };
        };
    };
};
var involution = function (dictEq) {
    return function (f) {
        return function (a) {
            return Data_Eq.eq(dictEq)(f(f(a)))(a);
        };
    };
};
var intransitive = function (dictHeytingAlgebra) {
    return function ($46) {
        return Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraFunction(Data_HeytingAlgebra.heytingAlgebraFunction(Data_HeytingAlgebra.heytingAlgebraFunction(dictHeytingAlgebra))))(transitive(dictHeytingAlgebra)($46));
    };
};
var identity = function (dictEq) {
    return function (op) {
        return function (id) {
            return function (a) {
                return Data_Eq.eq(dictEq)(op(a)(id))(a);
            };
        };
    };
};
var idempotent$prime = function (dictEq) {
    return function (op) {
        return function (a) {
            return Data_Eq.eq(dictEq)(op(a)(a))(a);
        };
    };
};
var idempotent = function (dictEq) {
    return function (op) {
        return function (a) {
            return Data_Eq.eq(dictEq)(op(op(a)))(a);
        };
    };
};
var flexibleIdentity = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return Data_Eq.eq(dictEq)(op(op(a)(b))(a))(op(a)(op(b)(a)));
            };
        };
    };
};
var falsehoodPreserving = function (dictHeytingAlgebra) {
    return function (dictEq) {
        return function (op) {
            return function (a) {
                return function (b) {
                    return Data_HeytingAlgebra.implies(dictHeytingAlgebra)(op(a)(b))(Data_HeytingAlgebra.disj(dictHeytingAlgebra)(a)(b));
                };
            };
        };
    };
};
var elasticIdentity = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return flexibleIdentity(dictEq)(op)(a)(b) && (jordanIdentity(dictEq)(op)(a)(b) && Data_Eq.eq(dictEq)(op(op(a)(op(a)(a)))(b))(op(op(a)(a))(op(a)(b))));
            };
        };
    };
};
var distributive = function (dictEq) {
    return function (f) {
        return function (g) {
            return function (a) {
                return function (b) {
                    return function (c) {
                        return leftDistributive(dictEq)(f)(g)(a)(b)(c) && rightDistributive(dictEq)(f)(g)(a)(b)(c);
                    };
                };
            };
        };
    };
};
var congruent = function (dictEq) {
    return function (f) {
        return function (a) {
            return function (b) {
                return Data_HeytingAlgebra.implies(Data_HeytingAlgebra.heytingAlgebraBoolean)(Data_Eq.eq(dictEq)(a)(b))(Data_Eq.eq(dictEq)(f(a))(f(b)));
            };
        };
    };
};
var commutative = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return Data_Eq.eq(dictEq)(op(a)(b))(op(b)(a));
            };
        };
    };
};
var cancellative = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return leftCancellative(dictEq)(op)(a)(b)(c) && rightCancellative(dictEq)(op)(a)(b)(c);
                };
            };
        };
    };
};
var asymmetric = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return Data_HeytingAlgebra.implies(Data_HeytingAlgebra.heytingAlgebraBoolean)(op(a)(b))(!op(b)(a));
            };
        };
    };
};
var associative = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return Data_Eq.eq(dictEq)(op(a)(op(b)(c)))(op(op(a)(b))(c));
                };
            };
        };
    };
};
var antitransitive = function (dictHeytingAlgebra) {
    return function (op) {
        return function (a) {
            return function (b) {
                return function (c) {
                    return Data_HeytingAlgebra.implies(dictHeytingAlgebra)(Data_HeytingAlgebra.conj(dictHeytingAlgebra)(op(a)(b))(op(b)(c)))(Data_HeytingAlgebra.not(dictHeytingAlgebra)(op(a)(c)));
                };
            };
        };
    };
};
var antisymmetric = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return Data_HeytingAlgebra.implies(Data_HeytingAlgebra.heytingAlgebraBoolean)(op(a)(b) && op(b)(a))(Data_Eq.eq(dictEq)(a)(b));
            };
        };
    };
};
var antireflexive = function (dictHeytingAlgebra) {
    return function (dictEq) {
        return function ($47) {
            return Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraFunction(dictHeytingAlgebra))(reflexive(dictHeytingAlgebra)(dictEq)($47));
        };
    };
};
var anticommutative = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return Data_Eq.notEq(dictEq)(op(a)(b))(op(b)(a));
            };
        };
    };
};
var alternativeIdentity = function (dictEq) {
    return function (op) {
        return function (a) {
            return function (b) {
                return leftAlternativeIdentity(dictEq)(op)(a)(b) && rightAlternativeIdentity(dictEq)(op)(a)(b);
            };
        };
    };
};
var absorbtion = function (dictEq) {
    return function (f) {
        return function (g) {
            return function (a) {
                return function (b) {
                    return Data_Eq.eq(dictEq)(f(a)(g(a)(b)))(a) && Data_Eq.eq(dictEq)(g(a)(f(a)(b)))(a);
                };
            };
        };
    };
};
module.exports = {
    absorbtion: absorbtion,
    leftAlternativeIdentity: leftAlternativeIdentity,
    rightAlternativeIdentity: rightAlternativeIdentity,
    alternativeIdentity: alternativeIdentity,
    anticommutative: anticommutative,
    antireflexive: antireflexive,
    asymmetric: asymmetric,
    antisymmetric: antisymmetric,
    antitransitive: antitransitive,
    associative: associative,
    leftCancellative: leftCancellative,
    rightCancellative: rightCancellative,
    cancellative: cancellative,
    commutative: commutative,
    congruent: congruent,
    elasticIdentity: elasticIdentity,
    leftDistributive: leftDistributive,
    rightDistributive: rightDistributive,
    distributive: distributive,
    falsehoodPreserving: falsehoodPreserving,
    flexibleIdentity: flexibleIdentity,
    idempotent: idempotent,
    "idempotent'": idempotent$prime,
    identity: identity,
    intransitive: intransitive,
    involution: involution,
    jacobiIdentity: jacobiIdentity,
    jordanIdentity: jordanIdentity,
    medial: medial,
    monotonic: monotonic,
    powerAssociative: powerAssociative,
    reflexive: reflexive,
    leftSemimedial: leftSemimedial,
    rightSemimedial: rightSemimedial,
    semimedial: semimedial,
    transitive: transitive,
    truthPreserving: truthPreserving,
    leftUnar: leftUnar,
    rightUnar: rightUnar,
    unipotent: unipotent,
    zeropotent: zeropotent
};
