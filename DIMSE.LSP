; Suppresses display of extension line
; ==============================================================
; Ver	Date		Description
; --------------------------------------------------------------
;		2025-11-26	daily
; 1.0	1999-06-01	Pick the demension object to hide the nearest extension line.
;					


(defun c:dse (/ olderr loop en ent obj typ se1 se2 pt)
	(setq loop t)
	(while loop
		(setq en (entsel "\nSelect dimension object: "))
		(if en
			(progn
				(setq
					ent	(entget (car en))
	 				obj	(cdr (assoc 0 ent))
					typ	(cdr (assoc 70 ent))
					se1	(cdr (assoc 13 ent))
					se2	(cdr (assoc 14 ent))
				)
				(if (and (= obj "DIMENSION") (member typ (list 0 1 32 33 160 161)))
					(progn
						(setq pt (cadr en))
						(if (< (distance pt se1) (distance pt se2))
							(setvar "dimse1" 1)
							(setvar "dimse2" 1)
						)
						(setvar "cmdecho" 0)
						(cmd "_dim1" "up" (car en) "")
						(setvar "cmdecho" 1)
						(setvar "dimse1" 0)
						(setvar "dimse2" 0)
					)
					(setq loop nil)
				)
			)
			(setq loop nil)
		)
	)
	(princ)
)

(princ "loaded. Start as C:DSE")
(princ)