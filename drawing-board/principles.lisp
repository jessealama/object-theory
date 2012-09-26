(axiom
 :name "Principle 1"
 (forall ((x object))
	 (implies (ordinary x)
		  (necessarily (not (exists ((F property))
					    (F x)))))))

(axiom
 :name "Definition of E-equal"
 (forall ((x object)
	  (y object))
	 (iff (E-equal x y)
	      (and (ordinary x)
		   (ordinary y)
		   (necessarily (forall ((F property))
					(iff (F x)
					     (F y))))))))

(axiom-scheme
 :name "Principle 3"
 (lambda (phi)
   (exists ((x object))
	   (and (abstract x)
		(forall ((F property))
			(iff (x F)
			     phi)))))
 :proviso
 (lambda (purported-instance)
   (let ((schematic-formula (exists ((?x object))
				    (and (abstract ?x)
					 (forall ((?F property))
						 (iff (?x ?F)
						      ?phi))))))
     (let ((substitution (unify purported-instance schematic-formula)))
       (when (not (eq substitution :fail))
	 (let ((x (value-in-substitution ?x substitution))
	       (F (value-in-substitution ?F substitution))
	       (phi (value-in-substitution ?phi)))
	   (and (not (free-in x phi))
		(subsetp (free-variables phi)
			 (list ?F)))))))))

(axiom
 :name "Principle 4"
 (forall ((x object)
	  (y object))
	 (iff (A-equal x y)
	      (and (abstract x)
		   (abstract y)
		   (necessarily (forall ((F property))
					(iff (x F)
					     (y F))))))))

(axiom
 :name "Principle 5"
 (forall ((x object)
	  (y object))
	 (iff (≈ x y)
	      (or (A≈ x y)
		  (E≈ x y)))))
