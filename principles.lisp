(axiom
 :name "Principle 1"
 :formula (forall ((x object))
		  (implies (ordinary x)
			   (necessarily (not (exists ((F property))
						     (F x)))))))

(axiom
 :name "Definition of E-equal"
 :formula (forall ((x object)
		   (y object))
		  (iff (E-equal x y)
		       (and (ordinary x)
			    (ordinary y)
			    (necessarily (forall ((F property))
						 (iff (F x)
						      (F y))))))))

(axiom-scheme
 :name "Principle 3"
 :scheme (lambda (phi)
	   (exists ((x object))
		   (and (abstract x)
			(forall ((F property))
				(iff (x F)
				     phi)))))
 :proviso (let ((existential-variables (bound-variables $scheme-instance))
		(matrix (matrix $scheme-instance)))
	    (let ((variable-and-type (first existential-variables))
		  (conjunct-rhs (rhs matrix)))
	      (let ((existential-variable (first variable-and-type))
		    (universal-matrix (matrix conjunct-hs)))
		(let ((equivalence-rhs (rhs universal-matrix)))
		  (not (free-in variable phi)))))))

(axiom
 :name "Principle 4"
 :formula (forall ((x object)
		   (y object))
		  (iff (A-equal x y)
		       (and (abstract x)
			    (abstract y)
			    (necessarily (forall ((F property))
						 (iff (x F)
						      (y F))))))))

(axiom
 :name "Principle 5"
 :formula (forall ((x object)
		   (y object))
		  (iff (equal x y)
		       (or (A-equal x y)
			   (E-equal x y)))))
