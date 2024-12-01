(defun varsave (varlist)
	(mapcar 
		'(lambda (itm) (list itm (getvar itm)))
		varlist
	)
	(setq fishlisp:var varlist)
)

(defun varrestore ()
	;(foreach itm fishlisp:var
	;	(apply 'setvar itm)
	;)
	(mapcar 'setvar fishlisp:var)
)

(defun varset (varlist / idx)
	(setq 
		fishlisp:var nil
		idx 0
	)
	(repeat (/ (length varlist) 2)
		(setq fishlisp:var
			(append fishlisp:var
				(list (list (nth idx varlist) (apply 'getvar (nth idx varlist))))
			)
		)
		(apply 'setvar (list (nth idx varlist) (nth (1+ idx) varlist)))
		(setq idx (+ 2 idx))
	)
)

(defun c:test ()
	(varset (list "cmdecho" 0 "clayer" "0" "plinewid" 255))
	(princ "\nSaved var: \n")
	(princ fishlisp:var)
	(princ "\Restor test\n")
	(varrestore)
	(princ)
)