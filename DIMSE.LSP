; Suppresses display of extension line
; ==============================================================
; Ver	Date		Description
; --------------------------------------------------------------
;		2025-11-26	daily
; 1.0	1999-06-01	Pick the demension object to hide the nearest extension line.
;					

; ens: entsel, list of point and entname
(defun suppress_by_ens (ens_ / ent se1 se2 pt)
	(setq
		pt (cadr ens_)
		ent	(entget (car ens_))
		se1	(cdr (assoc 13 ent))
		se2	(cdr (assoc 14 ent))
	)
	(if (< (distance pt se1) (distance pt se2))
		(setvar "dimse1" 1)
		(setvar "dimse2" 1)
	)
	(setvar "cmdecho" 0)
	(cmd "_dim1" "up" (car ens_) "")
	(setvar "cmdecho" 1)
	(setvar "dimse1" 0)
	(setvar "dimse2" 0)
)


(defun c:dse (/ loop ens ent obj typ)
	(setq loop t)
	(while loop
		(setq ens (entsel "\nSelect dimension object: "))
		(if ens
			(setq
				ent	(entget (car ens))
				obj	(cdr (assoc 0 ent))
				typ	(cdr (assoc 70 ent))
			)
			(setq loop nil)
		)
		(if (and ens (= obj "DIMENSION") (member typ (list 0 1 32 33 160 161)))
			(suppress_by_ens ens)
			(setq loop nil)
		)
	)
	(princ)
)

(princ "loaded. Start as C:DSE")
(princ)