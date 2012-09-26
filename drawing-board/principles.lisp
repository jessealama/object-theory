(reserve x object)
(reserve y object)
(reserve F property)

(axiom
 :name "Principle 1"
 (⟶ (E! x) (□ (∄ F (F x)))))

(axiom
 :name "Definition of E-equal"
 (⟷ (E≈ x y)
    (∧ (E! x)
	 (E! y)
	 (□ (∀ F (⟷ (F x) (F y)))))))

(definition
    (let ((x object)))
    (O! x)
  :means
  (◇ (E! x)))

(definition
    (let ((x object)))
    (A! x)
  :means
  (¬ (◇ (E! x))))

(axiom-scheme
 :name "Principle 3"
 (∃ ((?x object))
    (∧ (A! ?x)
       (∀ F
	  (⟷ (?x F) ?φ))))
 :proviso
 (and (not (free-in ?x ?φ))
      (subsetp (free-variables ?φ) (list ?x))))

(axiom
 :name "Principle 4"
 (⟷ (A≈ x y)
    (∧ (A! x)
       (A! y)
       (□ (∀ F (⟷ (x F) (y F)))))))

(axiom
 :name "Principle 5"
 (⟷ (≈ x y)
    (∨ (A≈ x y)
       (E≈ x y))))
