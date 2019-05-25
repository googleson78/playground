module Drunks where

open import Data.Empty using (⊥; ⊥-elim)
open import Data.Unit using (⊤; tt)
open import Relation.Nullary using (¬_)
open import Data.Sum using (_⊎_; inj₁; inj₂)


data Σ (A : Set) (B : A → Set) : Set where
  ⟨_,_⟩ : (x : A) → B x → Σ A B

Σ-syntax = Σ
infix 2 Σ-syntax
syntax Σ-syntax A (λ x → B) = Σ[ x ∈ A ] B

Σ-elim : ∀{A B : Set} {P : A → Set}
       → Σ[ x ∈ A ] (P x)
       → (∀(x : A) → P x → B)
       → B
Σ-elim ⟨ x , Px ⟩ f = f x Px

Inhabited : ∀(A : Set) → Set
Inhabited A = ⊤ → A

postulate
  stab : ∀{A : Set} → ¬ ¬ A → A
  lem : ∀{A : Set} → ¬ A ⊎ A

-- magically generate a witness from ¬∀
¬∀-cimplies-∃¬ : ∀{A : Set} {P : A → Set} → ¬ (∀(x : A) → P x) → Σ[ x ∈ A ] (¬ (P x))
¬∀-cimplies-∃¬ = λ z → stab (λ z₁ → z (λ x → stab (λ z₂ → z₁ ⟨ x , z₂ ⟩)))

-- human (mine) proof of drunks theorem
-- of course we need A to be inhabited in the first place
drunks : ∀{A : Set} {P : A → Set} → Inhabited A → Σ[ x ∈ A ] (P x → ∀(y : A) → P y)
drunks {A} {P} witnessA
  with lem {∀(x : A) → P x}
  -- either it's not true for everyone and we can always pick a person for whom the ancedent is false
...  | inj₁ ¬all = Σ-elim (¬∀-cimplies-∃¬ ¬all) (λ x ¬Px → ⟨ x , (λ Px _ → ⊥-elim (¬Px Px)) ⟩)
  -- or it's true for everyone, meaning we can pick whomever we want as a witness
...  | inj₂ all = ⟨ witnessA tt , (λ _ → all) ⟩

-- agda - lord of the stab
drunks' : ∀{A : Set} {P : A → Set} → Inhabited A → Σ[ x ∈ A ] (P x → ∀(y : A) → P y)
drunks' {A} {P} witnessA = stab
                             (λ z →
                                z
                                ⟨ witnessA tt ,
                                (λ x x₁ → stab (λ z₁ → z ⟨ x₁ , (λ x₂ x₃ → stab (λ _ → z₁ x₂)) ⟩))
                                ⟩)
