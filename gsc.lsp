; FISHLISP
; C:GSC
; History:
; 2003-08-19v0.0Origenal version.

(defun c:gsc ()
	(if ;fac
		(if ;pt0
			(if ;ss
				(setq ss (ssget))
				(setq pt0 (getpoint "\nSpecify base point:"))
			);if ss
			(progn
				(initget "Reference")
				(setq fac (getreal "\nSpecify scale factor or [Reference]: "))
			);progn
		);if pt0
		(progn
			(if (= fac "Reference")
				(progn
					(setq dis1 (if (setq dis1 (getdist "\nSpecify reference length <1>: ")) dis1 1))
					(setq dis2 (getdist pt0 "Specify new length:"))
					(setq fac (/ dis2 dis1))
				)
			);if ref
			(setq idx 0)
			(repeat (sslength ss)
				(setq 
					ent (entget (setq en (ssname ss idx)))
					pt1 (cdr (assoc 10 ent))
					pt2 (mapcar '(lambda (p1 p0) (+ p0 (* fac (- p1 p0)))) pt1 pt0)
					idx (1+ idx)
				)
				(setvar "cmdecho" 0)
				(cmd ".move" en "" pt1 pt2)
			);repeat
		); progn
	); if fac
	(princ)
);defun


