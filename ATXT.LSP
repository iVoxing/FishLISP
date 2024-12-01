; 1998-02-23	v2.0	Sepcify angle. Treat block.
; 1998-02-20	v1.0

(defun c:atxt (/ olderr ss ent amt idx a ang)
	(setq olderr *error*)
	(defun *error* (s)
		(if	(= s "Function cancelled")
			(princ (strcat "\nC:ATXT error: " s))
		)
		(setq *error* (if olderr
			olderr
			*pub_err*
		))
		(princ)
	)
	(princ "\nFishLISP. Align text/block entities horizontal. ")
	(if atxt_ang
		nil
		(setq atxt_ang 0.0)
	)
	(setvar "cmdecho" 0)
	(cmd "zoom" "1x")
	(setvar "cmdecho" 1)
	(setq ss (ssget '((0 . "text,insert"))))
	(if ss
		(setq 
			amt (sslength ss)
			idx 0
		)
		(setq amt 0)
	)
	(if ss
		(progn
			(princ "\nAngle: <")
			(princ (/ (* 180.0 atxt_ang) pi))
			(setq a (getreal "> "))
			(if a
				(setq ang (/ (* pi a) 180.0))
				(setq ang atxt_ang)
			)
			(setq atxt_ang ang)
		)
	)
	(repeat amt
		(setq ent (entget (ssname ss idx)))
		(entmod (subst (cons 50 ang) (assoc 50 ent) ent))
		(setq idx (1+ idx))
	)
	(setq *error* olderr)
	(princ)
)

(princ "loaded. Start as C:ATXT ")
(princ)