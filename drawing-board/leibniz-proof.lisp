;; Let's try to think of a nice way to represent the proof of Theorem
;; 4 of "A (Leibnizian) theory of concepts", p. 21.

(definition
    (let ((x obect)
	  (y object)))
    (real-sum x y)
  :means
  (and (concept it)
       (forall ((F property))
	       (iff [z F] (or [x F]
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
  (and (concept it)
       (forall ((F property))
	       (iff [x F]
		    (implies G F))))
  :existence
  ;; missing proof
  :uniqueness
  ;; missing proof
  )

(reserve G property)
(reserve H property)

(theorem
 (= (real-sum (concept-of G)
	      (concept-of H))
    (the ((x object))
      (and (concept x)
	   (forall ((F property))
		   (iff [x F]
			(or (implies G F)
			    (implies H F)))))))
 (proof
  (set description
       (the ((x object))
	 (and (concept x)
	      (forall ((F property))
		      (iff [x F]
			   (or (implies G F)
			       (implies H F)))))))
  (set cG (concept-of G))
  (set cH (concept-of H))
  (set sum (real-sum cH cH))
  (:A (forall ((P property)) [sum P]))
  (:B (forall ((P property)) [sum P]))
  (:C (A-equal sum description) :by :A :B :def-A-equal)
  (hence thesis :C :def-equality)))
