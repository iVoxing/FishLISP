(defun plwsc (en / vtxent vtxwd1 vtxwd2)
	(setq vtxent (entget (entnext en)))
	(if wid
	(while (/= (cdr (assoc 0 vtxent)) "SEQEND")
		(setq 
			vtxwd1 (cdr (assoc 40 vtxent))
			vtxwd2 (cdr (assoc 41 vtxent))
		)
		(if vtxwd1 
			(entmod (setq vtxent (subst (cons 40 wid) (assoc 40 vtxent) vtxent)))
			(entmod (setq vtxent (append vtxent (list (cons 40 wid)))))
		)
		(if vtxwd2 
			(entmod (setq vtxent (subst (cons 41 wid) (assoc 41 vtxent) vtxent)))
			(entmod (setq vtxent (append vtxent (list (cons 41 wid)))))
		)
		(setq vtxent (entget (entnext (cdr (assoc -1 vtxent)))))
	)
	)
	(if sc
	(while (/= (cdr (assoc 0 vtxent)) "SEQEND")
		(setq 
			vtxwd1 (cdr (assoc 40 vtxent))
			vtxwd2 (cdr (assoc 41 vtxent))
		)
		(if vtxwd1 
			(entmod (setq vtxent (subst (cons 40 (* vtxwd1 sc)) (assoc 40 vtxent) vtxent)))
		)
		(if vtxwd2 
			(entmod (setq vtxent (subst (cons 41 (* vtxwd2 sc)) (assoc 41 vtxent) vtxent)))
		)
		(setq vtxent (entget (entnext (cdr (assoc -1 vtxent)))))
	)
	)
	(entupd en)
)

(defun c:plwsc (/ ss idx en)
	(princ "\nFishLISP, Dec 10 1996")
	(princ "\nChange polylines' width, ver 2.0.")
	(princ "\nSelect polylinea to change width: ")
	(setq ss (ssget '((0 . "polyline,lwpolyline"))) idx 0)
	(if ss
		(progn
			(initget "Scale")
			(setq wid (getdist "\n[width Scale factor] <Enter new width for all segments>: "))
			(if (= wid "Scale") (setq wid nil sc (getreal "\nScale factor: ")))
		)
	)
	(if ss 
		(repeat (sslength ss)
			(setq en (ssname ss idx))
			(plwsc en)
			(setq idx (1+ idx))
		)
	)
	(princ)
)

(princ "\loaded. Start as C:PLWSC")
(princ)