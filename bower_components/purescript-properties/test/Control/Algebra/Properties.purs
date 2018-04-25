module Test.Control.Algebra.Properties where

import Prelude (class BooleanAlgebra, class Show, Unit, bind, discard, mod,
                negate, not, zero, ($), (&&), (*), (+), (-), (/), (<), (<=), (==), (||))
import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import Data.Either (isLeft)
import Data.HeytingAlgebra (implies)
import Data.NonEmpty ((:|))
import Test.QuickCheck (class Arbitrary, class Testable)
import Test.QuickCheck.Gen (elements)
import Control.Algebra.Properties
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (QCRunnerEffects, quickCheck)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (run)


type Op1 a = a → a
type Op2 a = a → a → a

data RPS = Rock | Paper | Scissors

defeats ∷ RPS → RPS → Boolean
defeats Rock Scissors = true
defeats Paper Rock = true
defeats Scissors Paper = true
defeats _ _ = false

instance showRPS ∷ Show RPS where
  show Rock = "Rock"
  show Paper = "Paper"
  show Scissors = "Scissors"

instance arbitraryRPS ∷ Arbitrary RPS where
  arbitrary = elements $ Rock :| [Paper, Scissors]


divisible ∷ Int → Int → Boolean
divisible a b = a `mod` b == zero

xor ∷ ∀ a. BooleanAlgebra a ⇒ a → a → a
xor a b = (a || b) && not (a && b)

iff ∷ ∀ a. BooleanAlgebra a ⇒ a → a → a
iff a b = (a `implies` b) && (b `implies` a)


quickCheckFail ∷ ∀ t e. Testable t ⇒ t → Aff (random ∷ RANDOM | e) Unit
quickCheckFail a = do
  result ← attempt $ quickCheck a
  isLeft result `shouldEqual` true


main ∷ Eff (QCRunnerEffects ()) Unit
main = run [consoleReporter] do
  describe "anticommutative" do
    describe "Int (-)" do
      it "should satisfy the property" do
        quickCheck $ anticommutative ((-) ∷ Op2 Int)
    describe "Int (+)" do
      it "should not satisfy the property" do
        quickCheckFail $ anticommutative ((+) ∷ Op2 Int)

  describe "antireflexive" do
    describe "Int (<)" do
      it "should satisfy the proeprty" do
        quickCheck $ antireflexive ((<) ∷ Int → Int → Boolean)
    describe "Boolean iff" do
      it "should not satisfy the proeprty" do
        quickCheckFail $ antireflexive (iff ∷ Op2 Boolean)

  describe "antisymmetric" do
    describe "Number (<=)" do
      it "should satisfy the property" do
        quickCheck $ antisymmetric ((<=) ∷ Number → Number → Boolean)
    describe "Int (==)" do
      it "should satisfy the property" do
        quickCheck $ antisymmetric ((==) ∷ Int → Int → Boolean)
    describe "Boolean (||)" do
      it "should not satisfy the property" do
        quickCheckFail $ antisymmetric ((||) ∷ Op2 Boolean)

  describe "antitransitive" do
    describe "RPS defeats" do
      it "should satisfy the property" do
        quickCheck $ antitransitive defeats
    describe "Boolean implies" do
      it "should not satisfy the property" do
        quickCheckFail $ antitransitive (implies ∷ Op2 Boolean)

  describe "absorbtion" do
    describe "Boolean (&&) (||)" do
      it "should satisfy the property" do
        quickCheck $ absorbtion ((&&) ∷ Op2 Boolean) (||)

  describe "associative" do
    describe "Int (+)" do
      it "should satisfy the property" do
        quickCheck $ associative ((+) ∷ Op2 Int)
    describe "Int (-)" do
      it "should not satisfy the property" do
        quickCheckFail $ associative ((-) ∷ Op2 Int)

  describe "cancellative" do
    describe "Boolean iff" do
      it "should satisfy the property" do
        quickCheck $ cancellative (iff ∷ Op2 Boolean)
    describe "Boolean implies" do
      it "should not satisfy the property" do
        quickCheckFail $ cancellative (implies ∷ Op2 Boolean)

  describe "commutative" do
    describe "Int (*)" do
      it "should satisfy the property" do
        quickCheck $ commutative ((*) ∷ Op2 Int)
    describe "Int (==)" do
      it "should satisfy the property" do
        quickCheck $ commutative ((==) ∷ Int → Int → Boolean)
    describe "Int (/)" do
      it "should not satisfy the property" do
        quickCheckFail $ commutative ((/) ∷ Op2 Int)

  describe "congruent" do
    describe "Boolean not" do
      it "should satisfy the property" do
        quickCheck $ congruent (not ∷ Op1 Boolean)

  describe "leftDistributive" do
    describe "Int (*) (+)" do
      it "should satisfy the property" do
        quickCheck $ leftDistributive ((*) ∷ Op2 Int) (+)
    describe "Int (+) (*)" do
      it "should not satisfy the property" do
        quickCheckFail $ leftDistributive ((+) ∷ Op2 Int) (*)

  describe "rightDistributive" do
    describe "Int (+) (*)" do
      it "should satisfy the property" do
        quickCheck $ rightDistributive ((+) ∷ Op2 Int) (*)
    describe "Int (*) (+)" do
      it "should not satisfy the property" do
        quickCheckFail $ rightDistributive ((*) ∷ Op2 Int) (+)

  describe "distributive" do
    describe "Boolean (&&) (||)" do
      it "should satisfy the property" do
        quickCheck $ distributive ((&&) ∷ Op2 Boolean) (||)
        quickCheck $ distributive ((||) ∷ Op2 Boolean) (&&)
    describe "Int (+) (*)" do
      it "should not satisfy the property" do
        quickCheckFail $ distributive ((+) ∷ Op2 Int) (*)

  describe "falsehoodPreserving" do
    describe "Boolean (||)" do
      it "should satisfy the property" do
        quickCheck $ falsehoodPreserving ((||) ∷ Op2 Boolean)
    describe "Boolean xor" do
      it "should not satisfy the property" do
        quickCheck $ falsehoodPreserving (xor ∷ Op2 Boolean)
    describe "Boolean iff" do
      it "should not satisfy the property" do
        quickCheckFail $ falsehoodPreserving (iff ∷ Op2 Boolean)

  describe "idempotent" do
    describe "Boolean not" do
      it "should satisfy the property" do
        quickCheck $ idempotent (not ∷ Op1 Boolean)

  describe "idempotent'" do
    describe "Boolean (&&)" do
      it "should satisfy the property" do
        quickCheck $ idempotent' ((&&) ∷ Op2 Boolean)
    describe "Boolean (||)" do
      it "should satisfy the property" do
        quickCheck $ idempotent' ((||) ∷ Op2 Boolean)
    describe "Boolean implies" do
      it "should not satisfy the property" do
        quickCheckFail $ idempotent' (implies ∷ Op2 Boolean)
    describe "Boolean xor" do
      it "should not satisfy the property" do
        quickCheckFail $ idempotent' (xor ∷ Op2 Boolean)
    describe "Boolean iff" do
      it "should not satisfy the property" do
        quickCheckFail $ idempotent' (iff ∷ Op2 Boolean)

  describe "identity" do
    describe "Boolean (&&) true" do
      it "should satisfy the property" do
        quickCheck $ identity ((&&) ∷ Op2 Boolean) true
    describe "Boolean (||) false" do
      it "should satisfy the property" do
        quickCheck $ identity ((||) ∷ Op2 Boolean) false
    describe "Int (*) 1" do
      it "should satisfy the property" do
        quickCheck $ identity ((*) ∷ Op2 Int) 1
    describe "Int (+) 0" do
      it "should satisfy the property" do
        quickCheck $ identity ((+) ∷ Op2 Int) 0

  describe "involution" do
    describe "Boolean not" do
      it "should satisfy the property" do
        quickCheck $ involution (not ∷ Op1 Boolean)
    describe "Int negate" do
      it "should satisfy the property" do
        quickCheck $ involution (negate ∷ Op1 Int)

  describe "jordanIdentity" do
    describe "Boolean (||)" do
      it "should satisfy the property" do
        quickCheck $ jordanIdentity ((||) ∷ Op2 Boolean)
    describe "Int (-)" do
      it "should satisfy the property" do
        quickCheck $ jordanIdentity ((-) ∷ Op2 Int)
    describe "Int (/)" do
      it "should satisfy the property" do
        quickCheck $ jordanIdentity ((/) ∷ Op2 Int)

  describe "medial" do
    describe "Int (-)" do
      it "should not satisfy the property" do
        quickCheck $ medial ((-) ∷ Op2 Int)
    describe "Int (/)" do
      it "should not satisfy the property" do
        quickCheckFail $ medial ((/) ∷ Op2 Int)

  describe "monotonic" do
    describe "Boolean (&&)" do
      it "should satisfy the property" do
        quickCheck $ monotonic ((&&) ∷ Op2 Boolean)
    describe "Boolean (||)" do
      it "should satisfy the property" do
        quickCheck $ monotonic ((||) ∷ Op2 Boolean)
    describe "Boolean xor" do
      it "should not satisfy the property" do
        quickCheckFail $ monotonic (xor ∷ Op2 Boolean)
    describe "Boolean iff" do
      it "should not satisfy the property" do
        quickCheckFail $ monotonic (iff ∷ Op2 Boolean)

  describe "powerAssociative" do
    describe "Int (*)" do
      it "should satisfy the property" do
        quickCheck $ powerAssociative ((*) ∷ Op2 Int)

  describe "reflexive" do
    describe "Boolean iff" do
      it "should satisfy the proeprty" do
        quickCheck $ reflexive (iff ∷ Op2 Boolean)
    describe "Int (<)" do
      it "should not satisfy the proeprty" do
        quickCheckFail $ reflexive ((<) ∷ Int → Int → Boolean)

  describe "transitive" do
    describe "Boolean implies" do
      it "should satisfy the property" do
        quickCheck $ transitive (implies ∷ Op2 Boolean)
    describe "Int divisible" do
      it "should satisfy the property" do
        quickCheck $ transitive divisible
    describe "Boolean (||)" do
      it "should not satisfy the property" do
        quickCheckFail $ transitive ((||) ∷ Op2 Boolean)

  describe "truthPreserving" do
    describe "Boolean iff" do
      it "should satisfy the property" do
        quickCheck $ truthPreserving (iff ∷ Op2 Boolean)
    describe "Boolean (&&)" do
      it "should satisfy the property" do
        quickCheck $ truthPreserving ((&&) ∷ Op2 Boolean)
    describe "Boolean xor" do
      it "should not satisfy the property" do
        quickCheckFail $ truthPreserving (xor ∷ Op2 Boolean)

  describe "unipotent" do
    describe "Boolean xor" do
      it "should satisfy the property" do
        quickCheck $ unipotent (xor ∷ Op2 Boolean)
    describe "Boolean (&&)" do
      it "should not satisfy the property" do
        quickCheckFail $ unipotent ((&&) ∷ Op2 Boolean)
