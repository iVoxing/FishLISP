
(defun c:dd (/ en ent e_type old_v new_v dcl_id e_lay laysta en_name bk_name bk_ins	att_ent att_ang att_pt10 att_pt11 att_wf att_dis1 bk_sc)
	(setvar "cmdecho" 0)
	(if (fl_check_ver 16)
		(setvar "dtexted" 1)
	)
	(setq en t)
	(while (setq en (entsel "\nPick a Text, Block or Dimension: "))
		(setq
			en_name (car en)
			ent	(entget en_name)
			e_type (cdr (assoc 0 ent))
			e_lay	(cdr (assoc 8 ent))
			laysta (cdr (assoc 70 (tblsearch "layer" e_lay)))
		)
		(cond
			((= e_type "TEXT")
				(cmd "_ddedit" en "")
			)
			((= e_type "MTEXT")
				(cmd "_ddedit" en "")
			)
			((= e_type "INSERT")
				(setq 
					bk_name (cdr (assoc 2 ent))
					bk_ins (cdr (assoc 10 ent))
					bk_scl (abs (cdr (assoc 41 ent)))
				)
				(cond
					((member (strcase bk_name) (list "AXI" "AXI0"))
						(setq	
							att_ent	(entget (entnext en_name))
							att_ang	(cdr (assoc 50 att_ent))
							old_v	(cdr (assoc 1 att_ent))
						)
						(ddf_dialog)
						(cond
							((not new_v))
							((= new_v old_v))
							((= new_v "")
								(entmod (subst (cons 1 new_v) (assoc 1 att_ent) att_ent))
								(entupd en_name)
							)
							((= (strlen new_v) (strlen old_v) 1)
								(entmod (subst (cons 1 new_v) (assoc 1 att_ent) att_ent))
								(entupd en_name)
							)				;block no change 1
							((and (> (strlen new_v) 1) (> (strlen old_v) 1))
								(entmod (subst (cons 1 new_v) (assoc 1 att_ent) att_ent))
								(entupd en_name)
							)				;block no change 2
							((= (strlen new_v) 1)
								(setq 
									att_pt10 (cdr (assoc 10 att_ent))
									att_dis1 (distance att_pt10 bk_ins)
									att_wf (cdr (assoc 41 att_ent))
									att_pt10 (polar att_pt10 att_ang (* (/ (* (- att_wf 1) (sqrt (- (expt att_dis1 2) (expt 2.25 2)))) att_wf) bk_scl))
								)
								(entmod (subst (cons 2 "axi0") (assoc 2 ent) ent))
								(setq 
									att_ent (entmod (subst (cons 72 4) (assoc 72 att_ent) att_ent))
									att_ent (entmod (subst (cons 1 new_v) (assoc 1 att_ent) att_ent))
									att_ent (entmod (subst (cons 10 att_pt10) (assoc 10 att_ent) att_ent))
									att_ent (entmod (subst (cons 11 bk_ins) (assoc 11 att_ent) att_ent))
									att_ent (entmod (subst (cons 41 1.0) (assoc 41 att_ent) att_ent))
								)
								(entupd en_name)
							)				; block change to axi0 no fit
							((= (strlen old_v) 1)
								(setq
									att_pt10 (polar bk_ins (+ att_ang (* pi 1.25)) (* 2.25 (sqrt 2) bk_scl))
									att_pt11 (polar bk_ins (+ att_ang (* pi 1.75)) (* 2.25 (sqrt 2) bk_scl))
								)
								(entmod (subst (cons 2 "axi") (assoc 2 ent) ent))
								(setq 
									att_ent (entmod (subst (cons 72 5) (assoc 72 att_ent) att_ent))
									att_ent (entmod (subst (cons 1 new_v) (assoc 1 att_ent) att_ent))
									att_ent (entmod (subst (cons 10 att_pt10) (assoc 10 att_ent) att_ent))
									att_ent (entmod (subst (cons 11 att_pt11) (assoc 11 att_ent) att_ent))
								)
								(entupd en_name)
							)				;block change to axi fit
						) ;cond compare new_v and old_v
					) ;cond axi treatment
					((member (strcase bk_name) (list "ELUR" "ELUL" "ELDR" "ELDL" "DIM_BG"))
						(setq	
							att_ent	(entget (entnext en_name))
							old_v	(cdr (assoc 1 att_ent))
						)
						(ddf_dialog)
						(cond
							((not new_v))
							;	((= new_v old_v))
							((member new_v (list "0.000" "0.00" "0.0" "0"))
								(setq new_v "%%P0.000")
								(entmod (subst (cons 1 new_v) (assoc 1 att_ent) att_ent))
								(entupd en_name)
							)
							((= new_v "")
								(setq 
									new_v (if el_h (rtos (/ (- (caddr (assoc 10 ent)) el_h) 1000.0) 2 3) new_v)
									new_v (if (member new_v (list "0.000" "0.00" "0.0" "0")) "%%P0.000" new_v)
								)
								(entmod (subst (cons 1 new_v) (assoc 1 att_ent) att_ent))
								(entupd en_name)
							)
							((member (strlen new_v) (list 1 2))
								(entmod (subst (cons 1 (strcat " " new_v " ")) (assoc 1 att_ent) att_ent))
								(entupd en_name)
							)
							((= (strlen new_v) 3)
								(entmod (subst (cons 1 (strcat " " new_v " ")) (assoc 1 att_ent) att_ent))
								(entupd en_name)
							)
							(t
								(entmod (subst (cons 1 new_v) (assoc 1 att_ent) att_ent))
								(entupd en_name)
							)
						)				;cond 
					); 
					(t				;cond other treatment
						(cmd "_ddatte" en)
					)
				) ;cond insert type
			) ; cond e_type = insert
			((= e_type "DIMENSION")
				(if (= laysta 68)
					(princ "\nThe entity is on a locked layer. ")
					(progn
						(setq old_v (cdr (assoc 1 (entget en_name))))
						(ddf_dialog)
						(if new_v
							(cmd "_dim1" "new" new_v en "")
						)
					)				;progn
				)				;if layer locked?
			)					;cond e_type=dimension
			(t)
		)					;cond
	)					;while
	(setvar "cmdecho" 1)
	(if (fl_check_ver 16)
		(setvar "dtexted" 0)
	)
	(princ)
	)

	(defun ddf_dialog ()
	(if (and (not dcl_id) (minusp (setq dcl_id (load_dialog "ddf_edit"))))
		(exit)
	)
	(if (not (new_dialog "ddf" dcl_id))
		(exit)
	)
	(set_tile "dim_edit" old_v)
	(action_tile
		"accept"
		(strcat "(progn (setq new_v (get_tile \"dim_edit\"))"
			"(done_dialog 1))"
		)
	)
	(action_tile "no" "(done_dialog 0)")
	(start_dialog)
)

(princ)