(defun ucs_e (flag_ / ucsf en et typ pt1 pt2 pt0)
	(setq ucsf (getvar "ucsfollow"))
	(setvar "cmdecho" 0)
	(setq en (entsel "\nSelect object: "))
	(if en
		(progn
			(setvar "ucsfollow" flag_)
			(setq 
				et (entget (car en))
				typ (cdr (assoc 0 et))
			)
			(cond
				((= typ "LINE")
					(setq 
						pt1 (trans (cdr (assoc 10 et)) 0 1)
						pt2 (trans (cdr (assoc 11 et)) 0 1)
						pt0 (cadr en)
					)
					(if (<= (distance pt0 pt1) (distance pt0 pt2))
						(cmd "ucs" "z" pt1 pt2)
						(cmd "ucs" "z" pt2 pt1)
					)
				)
				(t
					(cmd "ucs" "e" (car en))
					(princ "\n1 is not LINE. ")
				)
			)
		)
	)
	(setvar "cmdecho" 1)
	(setvar "ucsfollow" ucsf)
)

(defun c:ue () (ucs_e 0))
(defun c:uef () (ucs_e 1))

(princ "loaded. Start as C:UE C:UEF")
(princ)
