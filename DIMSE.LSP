; Suppresses display of extension line
; ==============================================================
; Version	Date	Description
; --------------------------------------------------------------
; 1.0		6-1-99	Pick the demension object to hide the
;			nearest extension line.
; next	wait	Keep the origenal extension line setting?
;			One-Step-Undo?
; ==============================================================
; Please visit my HomePage: orca.163.net
; orca.533.net

(defun c:dse (/ olderr loop en ent obj typ se1 se2 pt)
	(prompt "\nFishLISP, 6-1-99. Ver 1.0")
	(setq olderr *error*)
	(defun *error* (s)
		(setvar "dimse1" 0)
		(setvar "dimse1" 0)
		(setvar "cmdecho" 1)
		(setq *error* olderr olderr nil)
		(princ)
	)
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
						(cmd "dim1" "up" (car en) "")
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
	(setq *error* olderr)
	(princ)
)

(princ "loaded. Start as C:DIMSE\nPlease visit my HomePage: orca@163.net\torca@zb169.net")
(princ)