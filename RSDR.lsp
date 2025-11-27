; 2025-11-27		New block system, base width is 1
;					Pure LISP, no insert nor scale now
; 2025-11-21		rewriting
; 2001-07-25 v1.1	修正了一个在AutoCAD2000中的一个字符大小写敏感问题
; 1998-07-16 v1.0	改变门的尺寸

(setq dr_list (list))
(foreach itm (list "DR01" "DR02" "DR03" "DR04" "DR05" "DR06" "DR07" "DR08")
	(if (findfile (strcat itm ".DWG"))
		nil 
		(setq dr_list (append dr_list (list itm))))
)
(if dr_list
	(progn
		(princ (strcat "\n" (fl_strlist_2_string dr_list ".dwg, ") "not found. Abort! "))
		(exit)
	)
)

(defun renew_lines (pt_ dist_ / new_pt)
	(setq new_pt (polar pt_ BKANG dist_))
	(mapcar
		'(lambda (code_ / ss_pt idx en pt ent)
			(setq
				ss_pt (ssadd)
				idx 0
			)
			(repeat (sslength ss0)
				(setq
					en (ssname ss0 idx)
					ent (entget en)
					pt (cdr (assoc code_ ent))
				)
				;; 必须使用 equal 进行筛选
				(if (equal pt pt_ 0.1)
					(entmod (subst (cons code_ new_pt) (assoc code_ ent) ent))
				)
				(setq idx (1+ idx))
			)
		)
		'(10 11)
	)
)

(defun check_dr (drname_)
	(if (tblsearch "block" drname_)
		nil
		(progn
			(cmd "_insert" drname_ "0,0" "1" "1" "0")
			(entdel (entlast))
		)
	)
)

(defun rescale_dr (en_list_ sc_ / ent)
	(mapcar 
		'(lambda (enn_)
			(setq ent (entget enn_))
			(mapcar 
				'(lambda (code_ / old_wid_pair)
					(setq 
						old_wid_pair (assoc code_ ent)
						ent (subst (cons code_ (* sc_ (cdr old_wid_pair))) old_wid_pair ent)
					)
				)
				'(41 42 43)
			)
			(entmod ent)
		)
		en_list_
	)
)

(defun replace_dr (ent_ bk_any_ / dr_en_list dr_renew dr_new)
	(setq dr_en_list (list (cdr (assoc -1 ent_))))
	(if (listp bk_any_)
		(setq dr_renew (car bk_any_) dr_new (cadr bk_any_))
		(setq dr_renew bk_any_)
	)
	(entmod (subst (cons 2 dr_renew) (assoc 2 ent_) ent_))
	(if dr_new
		(progn
			(entmake (subst (cons 2 dr_new) (assoc 2 ent_) ent_))
			(setq dr_en_list (append dr_en_list (list (entlast))))
		)
	)
	dr_en_list
)


(defun resize_dr (en_ / ent bkname bkins old_wid BKANG new_wid f_pt1 f_pt2 ss_cap cap1 cap2 pt1 pt2 pt3 pt4 dst ss0 en_kpt)
	(setq 
		ent 	(entget en_)
		dr_list (list en_)
		en_kpt	nil
		bkname 	(strcase (cdr (assoc 2 ent)))
		bkins	(cdr (assoc 10 ent))
		old_wid	(abs (cdr (assoc 41 ent)))
		BKANG	(cdr (assoc 50 ent))
	)
	(setq new_wid (getint (strcat "\nWidth: <" (rtos old_wid 2 0) "> ")))
	(if (and new_wid (/= new_wid old_wid))
		(setq
			sc (/ new_wid old_wid)
			f_pt1 (polar bkins BKANG (+ (* old_wid 0.5) 50))
			f_pt2 (polar bkins BKANG (- (+ (* old_wid 0.5) 50)))
			ss_cap (ssget "f" (list f_pt1 f_pt2))
		)
	)
	(if ss_cap
		(setq ss_cap (ssget "p" (list (cons 8 FLLT_WALL) '(0 . "LINE"))))
	)
	(if ss_cap
		(setq 
			cap1 (entget (ssname ss_cap 0))
			cap2 (entget (ssname ss_cap 1))
			pt1 (cdr (assoc 10 cap1))
			pt2 (cdr (assoc 11 cap1))
			pt3 (cdr (assoc 10 cap2))
			pt4 (cdr (assoc 11 cap2))
			dst (/ (- new_wid old_wid) 2)
			ss0 (ssget "cp" (list pt1 pt2 pt4 pt3))
		)
	)
	(if ss0
		(setq ss0 (ssget "p" (list '(0 . "LINE") (cons 8 FLLT_WALL))))
	)

	(if ss0
		(progn
			(renew_lines pt1 dst)
			(renew_lines pt2 dst)
			(renew_lines pt3 (- dst))
			(renew_lines pt4 (- dst))
		)
	)

	(if ss0
		(cond 
			((= bkname "DR01")
				(cond 
					((<= new_wid 1000)
						(setq dr_list (replace_dr ent '("DR03" "DR04")))
					)
					((>= new_wid 3000)
						(setq dr_list (replace_dr ent "DR07"))
					)
				)
			)
			((= bkname "DR02")
				(cond 
					((<= new_wid 1000)
						(setq dr_list (replace_dr ent '("DR05" "DR06")))
					)
					((>= new_wid 3000)
						(setq dr_list (replace_dr ent "DR08"))
					)
				)
			)
			((member bkname (list "DR03" "DR04"))
				(cond
					((> 3000 new_wid 1000)
						(setq dr_list (replace_dr ent "DR01"))
					)
					((>= new_wid 3000)
						(setq dr_list (replace_dr ent "DR07"))
					)
				)
			)
			((member bkname (list "DR05" "DR06"))
				(cond
					((> 3000 new_wid 1000)
						(setq dr_list (replace_dr ent "DR02"))
					)
					((>= new_wid 3000)
						(setq dr_list (replace_dr ent "DR08"))
					)
				)
			)
			((= bkname "DR07")
				(cond 
					((<= new_wid 1000)
						(setq dr_list (replace_dr ent '("DR03" "DR04")))
					)
					((> 3000 new_wid 1000)
						(setq dr_list (replace_dr ent "DR01"))
					)
				)
			)
			((= bkname "DR08")
				(cond 
					((<= new_wid 1000)
						(setq dr_list (replace_dr ent '("DR05" "DR06")))
						;(setq en_kpt t)
					)
					((> 3000 new_wid 1000)
						(setq dr_list (replace_dr ent "DR02"))
					)
				)
			)
			(t)
		)
	)

	(if ss0 (rescale_dr dr_list sc))

	(if (> (length dr_list) 1)
		(setq en_kpt (entsel "\n>>Door to be kept: "))
	)
	(if en_kpt (setq en_kpt (car en_kpt)))
	(if en_kpt
		(mapcar
			'(lambda (enn_)
				(if (eq enn_ en_kpt)
					nil
					(entdel enn_)
				)
			)
			dr_list
		)
	)
)

(defun c:rsdr (/ en enn ent loop)
	(princ "\nResize Doors.")
	(mapcar 'check_dr (list "DR01" "DR02" "DR03" "DR04" "DR05" "DR06" "DR07" "DR08"))
	(setvar "cmdecho" 0)

	(setq loop t)
	(while loop
		(setq en (entsel "\nPick a DR insert: "))
		(if en
			(setq 
				enn (car en)
				ent (entget enn)
			)
		)
		(cond
			((not en)
				(setq loop nil)
			)
			((/= (cdr (assoc 0 ent)) "INSERT")
				(princ "\n1 was not an Insert. ")
			)
			((/= (strcase (substr (cdr (assoc 2 ent)) 1 3)) "DR0")
				(princ "\n1 was not DR0 insert. ")
			)
			(t
				(fl_undo_begin)
				(resize_dr enn)
				(fl_undo_end)
			)
		)
	)
	(setvar "cmdecho" 1)
	(princ)
)

(princ "loaded. Start as C:RSDR. ")
(princ)