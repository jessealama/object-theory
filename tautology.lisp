;;; tautologies.lisp -- Check whether a formula is a propositional tautology

(in-package :object-theory)

(defgeneric leftmost-atom (formula))

(defmethod leftmost-atom ((formula (eql top)))
  nil)

(defmethod leftmost-atom ((formula (eql contradiction)))
  nil)

(defmethod leftmost-atom ((formula atomic-formula))
  formula)

(defmethod leftmost-atom ((formula unary-connective-formula))
  (leftmost-atom (argument formula)))

(defmethod leftmost-atom ((formula binary-connective-formula))
  (let ((left (leftmost-atom (lhs formula))))
    (if (null left)
	(leftmost-atom (rhs formula))
	left)))

(defgeneric reduce-top-and-bottom-formula (formula))

(defmethod reduce-top-and-bottom-formula ((formula (eql top)))
  top)

(defmethod reduce-top-and-bottom-formula ((formula (eql contradiction)))
  contradiction)

(defmethod reduce-top-and-bottom-formula ((formula negation))
  (let ((reduced (reduce-top-and-bottom-formula (unnegate formula))))
    (if (eq reduced top)
	contradiction
	top)))

(defmethod reduce-top-and-bottom-formula ((formula binary-conjunction))
  (let ((reduced-lhs (reduce-top-and-bottom-formula (lhs formula))))
    (if (eq reduced-lhs contradiction)
	contradiction
	(reduce-top-and-bottom-formula (rhs formula)))))

(defmethod reduce-top-and-bottom-formula ((formula binary-disjunction))
  (let ((reduced-lhs (reduce-top-and-bottom-formula (lhs formula))))
    (if (eq reduced-lhs top)
	top
	(reduce-top-and-bottom-formula (rhs formula)))))

(defmethod reduce-top-and-bottom-formula ((formula implication))
  (let ((reduced-antecedent (reduce-top-and-bottom-formula (antecedent formula))))
    (if (eq reduced-antecedent contradiction)
	top
	(reduce-top-and-bottom-formula (consequent formula)))))

(defmethod reduce-top-and-bottom-formula ((formula equivalence))
  (let ((reduced-lhs (reduce-top-and-bottom-formula (lhs formula)))
	(reduced-rhs (reduce-top-and-bottom-formula (rhs formula))))
    (if (eq reduced-lhs reduced-rhs)
	top
	contradiction)))

(defun tautology? (formula)
  (let ((first-atom (leftmost-atom formula)))
    (if (null first-atom) ;; only tops and bottoms remain
	(eq (reduce-top-and-bottom-formula formula) top)
	(let ((subst-true (acons first-atom top nil))
	      (subst-false (acons first-atom contradiction nil)))
	  (let ((formula-with-atom-true (apply-substitution subst-true
							    formula
							    :test #'equal-atomic-formulas?))
		(formula-with-atom-false (apply-substitution subst-false
							     formula
							     :test #'equal-atomic-formulas?)))
	    (and (tautology? formula-with-atom-true)
		 (tautology? formula-with-atom-false)))))))

;;; tautology.lisp ends here
