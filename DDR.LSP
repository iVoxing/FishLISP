; Change direction of Door
; 2025-11-27	New block system, base width is 1
;				pure LISP
; 1997-12-24

(defun ddr_check (en_ / ent bkname bkins bksc bkang dw_xdt bklist clay)
	(setq 
		ent 	(entget en_)
		bkname	(strcase (cdr (assoc 2 ent)))
		bkins	(cdr (assoc 10 ent))
		bksc	(cdr (assoc 41 ent))
		bkang	(/ (* (cdr (assoc 50 ent)) 180.0) pi)
	)
	(setq clay (getvar "clayer"))
	(setvar "clayer" FLLT_DOOR)
	(cond
		((member bkname (list "DR01" "DR02" "DR07" "DR08"))
			(reins_dr_1 en_ bkname)
		)
		((member bkname (setq bklist (list "DR03" "DR04" "DR05" "DR06")))
			(reins_dr_2 en_ bkname)
		)
		(t 
			(princ "\n1 was unknown Door type. ")
		)
	)
	(setvar "clayer" clay)
)

(defun reins_dr_1 (en_ name_ / ent dr_new)
	(setq ent (entget en_))
	(cond
		((= name_ "DR01") (setq dr_new "DR02"))
		((= name_ "DR02") (setq dr_new "DR01"))
		((= name_ "DR07") (setq dr_new "DR08"))
		((= name_ "DR08") (setq dr_new "DR07"))
	)
	(entmod (subst (cons 2 dr_new) (assoc 2 ent) ent))
)

(defun reins_dr_2 (en_ name_ / ent as2 drlist)
	(setq
		ent (entget en_)
		as2	(assoc 2 ent)
		drlist (list en_)
	)
	(foreach dr_ bklist
		(if (= dr_ name_)
			nil
			(progn
				(entmake (subst (cons 2 dr_) as2 ent))
				(setq drlist (append drlist (list (entlast))))
			)
		)
	)
	(if (setq en_kpt (entsel "\n>>Pick the door to be kept: "))
		(setq en_kpt (car en_kpt))
	)
	(if en_kpt
		(mapcar
			'(lambda (enn_)
				(if (eq enn_ en_kpt)
					nil
					(entdel enn_)
				)
			)
			drlist
		)
	)
)

(defun c:ddr (/ en loop)
	(setq *error* *pub_error*)
	(setvar "cmdecho" 0)
	(setq loop t)
	(while loop
		(setq en (entsel "\nPick a Door insert: "))
		(cond
			((not en)
				(setq loop nil)
			)
			((/= (cdr (assoc 0 (entget (car en)))) "INSERT")
				(princ "\n1 was not an Insert. ")
			)
			((/= (strcase (substr (cdr (assoc 2 (entget (car en)))) 1 3)) "DR0")
				(princ "\n1 was unknown Door type. ")
			)
			( t
				(ddr_check (car en))
			)
		)
	)
	(setvar "cmdecho" 1)
	(princ)
)

(princ "loaded. Start as C:DDR. ")
(princ)