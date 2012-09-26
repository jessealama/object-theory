;; Let's try to think of a nice way to represent the proof of Theorem
;; 4 of "A (Leibnizian) theory of concepts", p. 21.

(reserve x object)
(reserve y object)
(reserve F property)
(reserve G property)
(reserve H property)
(reserve P property)

(definition
  (let ((x object))
  (concept x)
  :means
  (A! x)))

(theorem
 :name "Theorem 1"
 (≈ x x)
 (proof
  (:A (A≈ x x)
      (proof
       (:B (A! x))
       (:C (□ (∀ F (⟷ [x F] [x F])))
	   (proof
	    (:D (∀ F (⟷ [x F] [x F]))
		(proof
		 (let F)
		 (⟷ [x F] [x F])
		 (hence thesis)))))
       ;; this isn't valid
       (hence thesis :by :B "Definition of A-equal")))
  (hence thesis :by "Principle 5")))

(theorem
 :name "Theorem 2"
 (⟶ (≈ x y)
    (≈ y x)))

(theorem
 :name "Theorem 3"
 (⟶ (∧ (≈ x y)
       (≈ y z))
    (≈ x z)))

(definition
    (let ((x obect)
	  (y object)))
    (⊕ x y)
  :means
  (∧ (concept it)
     (∀ F (⟷ [z F]
	       (∨ [x F]
		  [z F]))))
  :existence
  ;; missing proof
  :uniqueness
  ;; missing proof
  )

(definition
    (let ((G property)))
    (concept-of G)
  :means
  (∧ (concept it)
       (∀ F (⟷ [x F]
	         (⊂ G F))))
  :existence
  ;; missing proof
  :uniqueness
  ;; missing proof
  )

(theorem
 (≈ (⊕ (concept-of G)
       (concept-of H))
    (℩ x
       (∧ (concept x)
	  (∀ F (⟷ [x F]
		    (∨ (⊂ G F)
		       (⊂ H F)))))))
 (proof
  (set description
       (℩ x
	  (∧ (concept x)
	     (∀ F (⟷ [x F]
		     (∨ (⊂ G F)
			(⊂ H F)))))))
  (set cG (concept-of G))
  (set cH (concept-of H))
  (set sum (⊕ cH cH))
  (:A (∀ P [sum P]))
  (:B (∀ P [sum P]))
  (:C (A≈ sum description) :by :A :B :def-A-equal)
  (hence thesis :C :def-equality)))
