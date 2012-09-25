;; Let's try to think of a nice way to represent the proof of Theorem
;; 4 of "A (Leibnizian) theory of concepts", p. 21.

(definition
    (let ((x object)
	  (y object))
      (real-sum x y)
      :means
      (and (concept it)
	   (forall ((F property))
		   (iff (z F)
			(or (x F)
			    (z F)))))
      :existence ;; some proof of existence here
      :uniqueness ;; some proof of uniqueness here
))

(definition
    (let ((G property))
      (concept G)
      :means
      (and (concept it)
	   (forall ((F property))
		   (iff (x F)
			(implies G F))))
      :existence ;; proof of existence here
      :uniqueness ;; proof of uniqueness here
      ))

(theorem
 (forall ((G property)
	  (H property))
	 (= (real-sum (concept-of G)
		      (concept-of H))
	    (the ((x object))
	      (and (concept x)
		   (forall ((F property))
			   (iff (x F)
				(or (implies G F)
				    (implies H F))))))))
 (proof
  (let ((G property)
	(H property))
    ;; abbreviations
    (set description
	 (the ((x object))
	   (and (concept x)
		(forall ((F property))
			(iff (x F)
			     (or (implies G F)
				 (implies H F)))))))
    (set cG (concept-of G))
    (set cH (concept-of H))
    (set sum (real-sum cH cH))
    (:A (forall ((P property))
		(sum P))
	;; some subproof
	)
    (:B (forall ((P property))
		(sum P))
	;; some subproof
	)
    (C: (A-equal sum description) :A :B :def-A-equal)
    (hence thesis :C :def-equality))))
