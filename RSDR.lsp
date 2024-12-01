; 1998-07-16v1.0改变门的尺寸
; 2001-07-25v1.1修正了一个在AutoCAD2000中的一个字符大小写敏感问题

(setq olderr *error*)

(defun *error* (s)
	(setq *error* olderr)
	(princ)
)

(setq dr_list (list))
(foreach itm (list "DR1" "DR2" "DR3" "DR4" "DR5" "DR6" "DR7" "DR8")
	(if (findfile (strcat itm ".DWG")) nil (setq dr_list (append dr_list (list itm))))
)
(if dr_list
	(progn
		(princ "\n")
		(foreach itm dr_list
			(if itm (princ (strcat itm ".DWG ")))
		)
		(princ "not found in search path. Application abort! ")
		(exit)
	)
)

(setq *error* olderr olderr nil)

(defun c:rsdr (/ olderr ucsf os undo_id en mod_lin dr_sz check_dr)
	(princ "\nFishLISP. 7-16-1998.")
	(princ "\nResize Doors. v1.1. ")

	(setvar "cmdecho" 0)
	(defun check_dr (drname)
		(if (tblsearch "block" drname)
			nil
			(progn
				(cmd "insert" drname "0,0" "1" "1" "0")
				(entdel (entlast))
			)
		)
	)

	(check_dr "DR1")
	(check_dr "DR2")
	(check_dr "DR3")
	(check_dr "DR4")
	(check_dr "DR5")
	(check_dr "DR6")
	(check_dr "DR7")
	(check_dr "DR8")

	(setq olderr *error*)
	(defun *error* (s)
		(setvar "cmdecho" 0)
		(fl_undo_end)
		(setvar "osmode" os)
		(setvar "cmdecho" 1)
		(setq *error* olderr olderr nil)
		(princ)
	)

	(setvar "cmdecho" 0)
	(fl_undo_begin)
	(setq ucsf (getvar "ucsfollow"))
	(setvar "ucsfollow" 0)
	(setvar "expert" 4)
	(setvar "cmdecho" 0)
	(cmd "ucs" "s""$temp$")
	(cmd "ucs" "w")

	(defun mod_lin (flag pt dist / ss_pt idx new_pt ent1)
		(cmd "select" ss0 "")
		(setq ss_pt (ssget "p" (list (cons flag pt))))
		(if ss_pt
			(progn
				(setq idx 0)
				(setq new_pt (polar pt bkang dist))
				(repeat (sslength ss_pt)
				(setq ent1 (entget (ssname ss_pt idx)))
				(entmod (subst (cons flag new_pt) (assoc flag ent1) ent1))
				(setq idx (1+ idx))
				)
			)
		)
	)

	(defun dr_sz (en / ent bkname bkins bksc bkang new_sc f_pt1 f_pt2 ss_cap cap1 cap2 pt1 pt2 pt3 pt4 dst ss0 en_kpt)
		(setq 
			ent 	(entget en)
			bkname 	(strcase (cdr (assoc 2 ent)))
			bkins	(cdr (assoc 10 ent))
			bksc	(abs (cdr (assoc 41 ent)))
			bkang	(cdr (assoc 50 ent))
		) 
		(princ (strcat "\nWidth: <" (rtos (* 1000 bksc)) "> "))
		(setq new_sc (getint))
		(if (and new_sc (/= new_sc (* bksc 1000)))
			(progn
				(setq new_sc (/ new_sc 1000.0))
				(cond 
					((= bkname "DR1")
						(cond 
							((<= new_sc 1.0)
								(entmake (subst (cons 2 "DR3") (assoc 2 ent) ent))
								(entmod (subst (cons 2 "DR4") (assoc 2 ent) ent))
								(setq en_kpt (entsel "\n>>Door to be kept: "))
								(if en_kpt
									(progn
										(setq en_kpt (car en_kpt))
										(cmd "erase" en (entlast) "r" en_kpt "")
									)
								)
							)
							((>= new_sc 3.0)
								(entmod (subst (cons 2 "DR7") (assoc 2 ent) ent))
							)
						)
					)
					((= bkname "DR2")
						(cond 
							((<= new_sc 1.0)
								(entmake (subst (cons 2 "DR5") (assoc 2 ent) ent))
								(entmod (subst (cons 2 "DR6") (assoc 2 ent) ent))
								(setq en_kpt (entsel "\n>>Door to be kept: "))
								(if en_kpt
									(progn
										(setq en_kpt (car en_kpt))
										(cmd "erase" en (entlast) "r" en_kpt "")
									)
								)
							)
							((>= new_sc 3.0)
								(entmod (subst (cons 2 "DR8") (assoc 2 ent) ent))
							)
						)
					)
					((member bkname (list "DR3" "DR4"))
						(cond
							((> 3.0 new_sc 1.0)
								(entmod (subst (cons 2 "DR1") (assoc 2 ent) ent))
							)
							((>= new_sc 3.0)
								(entmod (subst (cons 2 "DR7") (assoc 2 ent) ent))
							)
						)
					)
					((member bkname (list "DR5" "DR6"))
						(cond
							((> 3.0 new_sc 1.0)
								(entmod (subst (cons 2 "DR2") (assoc 2 ent) ent))
							)
							((>= new_sc 3.0)
								(entmod (subst (cons 2 "DR8") (assoc 2 ent) ent))
							)
						)
					)
					((= bkname "DR7")
						(cond 
							((<= new_sc 1.0)
								(entmake (subst (cons 2 "DR3") (assoc 2 ent) ent))
								(entmod (subst (cons 2 "DR4") (assoc 2 ent) ent))
								(setq en_kpt (entsel "\n>>Door to be kept: "))
								(if en_kpt
									(progn
										(setq en_kpt (car en_kpt))
										(cmd "erase" en (entlast) "r" en_kpt "")
									)
								)
							)
							((> 3.0 new_sc 1.0)
								(entmod (subst (cons 2 "DR1") (assoc 2 ent) ent))
							)
						)
					)
					((= bkname "DR8")
						(cond 
							((<= new_sc 1.0)
								(entmake (subst (cons 2 "DR5") (assoc 2 ent) ent))
								(entmod (subst (cons 2 "DR6") (assoc 2 ent) ent))
								(setq en_kpt (entsel "\n>>Door to be kept: "))
								(if en_kpt
									(progn
										(setq en_kpt (car en_kpt))
										(cmd "erase" en (entlast) "r" en_kpt "")
									)
								)
							)
							((> 3.0 new_sc 1.0)
								(entmod (subst (cons 2 "DR2") (assoc 2 ent) ent))
							)
						)
					)
				)
				(cmd "scale" (if en_kpt en_kpt en) "" bkins "r" bksc new_sc)
				(setq en_kpt nil)
				(setq 
					f_pt1 (polar bkins bkang (+ (* bksc 500) 50))
					f_pt2 (polar bkins bkang (- (+ (* bksc 500) 50)))
					ss_cap (ssget "f" (list f_pt1 f_pt2))
					ss_cap (ssget "p" (list (cons 8 FLLT_WALL) '(0 . "line")))
					cap1 (entget (ssname ss_cap 0))
					cap2 (entget (ssname ss_cap 1))
					pt1 (cdr (assoc 10 cap1))
					pt2 (cdr (assoc 11 cap1))
					pt3 (cdr (assoc 10 cap2))
					pt4 (cdr (assoc 11 cap2))
					dst (/ (- new_sc bksc) 0.002)
					ss0 (ssget "cp" (list pt1 pt2 pt4 pt3))
					ss0 (ssget "p" (list '(0 . "line") (cons 8 FLLT_WALL)))
				)
				(mod_lin 10 pt1 dst)
				(mod_lin 11 pt1 dst)
				(mod_lin 10 pt2 dst)
				(mod_lin 11 pt2 dst)
				(mod_lin 10 pt3 (- dst))
				(mod_lin 11 pt3 (- dst))
				(mod_lin 10 pt4 (- dst))
				(mod_lin 11 pt4 (- dst))
			)
		)
	)

	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(setvar "cmdecho" 0)
	(setq undo_id 0)
	(setq loop t)
	(while loop
		(initget "Undo")
		(setq en (entsel "\n[Undo]<Pick a Door>: "))
		(cond
			((= en "Undo")
				(if (< undo_id 1)
					(princ "\nCommand has been completely undone. ")
					(progn
						(fl_undo_end)
						(cmd "undo" "")
						(setq undo_id (1- undo_id))
					)
				)
			)
			((not en) (setq loop nil))
			((/= (cdr (assoc 0 (entget (car en)))) "INSERT")
				(princ "\n1 was not an Insert. ")
			)
			((/= (strcase (substr (cdr (assoc 2 (entget (car en)))) 1 2)) "DR")
				(princ "\n1 was unknown Door type. ")
			)
			(t
				(fl_undo_end)
				(fl_undo_begin)
				(dr_sz (car en))
				(fl_undo_end)
				(setq undo_id (1+ undo_id))
			)
		)
	)

	(fl_undo_begin)
	(setvar "cmdecho" 0)
	(cmd "ucs" "r" "$temp$")
	(cmd "ucs" "d" "$temp$")
	(setvar "ucsfollow" ucsf)
	(fl_undo_end)
	(setvar "expert" 0)
	(setvar "osmode" os)
	(setvar "cmdecho" 1)
	(setq *error* olderr)
	(princ)
)

(princ "loaded. Start as C:RSDR. ")
(princ)