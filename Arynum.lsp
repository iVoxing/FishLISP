; This LISP application is a good sample of
; function "subst" "cons" and "entmake"
;
;* C:Arynum	* Array nummber text
; 2025-11-13	rewrite, tested ok

(defun c:arynum (/ ent num0 pt0 x0 y0 ay_dir ay_num ar_dis nm_add count x1 y1 num1 txt1 pt1 ent1 x_dis y_dis)
	(princ "\nArray Nummber Text. FishLISP. Dec 14 1995")
	(prompt "\nSelect base nummber TEXT to edit:")
    (setq no_selection T)
	(while no_selection
		(setq ent (entget (car (entsel))))
		(if (= (cdr (assoc 0 ent)) "TEXT")
			(setq no_selection nil)
			(princ "\nSelection is not TEXT.")
		)
	)
	(setq 
		num0 (read (cdr (assoc 1 ent)))
		pt0 (trans (cdr (assoc 10 ent)) 0 1)
		x0 (car pt0) y0 (cadr pt0)
	)
 
	(setq ay_dir (getstring "\nArray direction--X or <Y>:"))
	(if (= ay_dir "") (setq ay_dir "y"))
	(setq ay_num (getint "\nAount to array:"))
	(setq ar_dis (getdist "\nArray distance:"))
	(setq nm_add (getreal "\nNummber add each:"))

	(setq count 1)
	(while (/= count ay_num)
		(if (= ay_dir "y")
			(setq x_dis 0 y_dis ar_dis)
			(setq x_dis ar_dis y_dis 0)
		)
		(setq 
            x1 (+ (* x_dis count) x0)
            y1 (+ (* y_dis count) y0)
			num1 (+ (* nm_add count) num0)
			txt1 (rtos num1 2 0)
			pt1 (trans (list x1 y1) 1 0)
		)
		(setq ent1 (subst (cons 1 txt1) (assoc 1 ent) ent))
		(setq ent1 (subst (cons 10 pt1) (assoc 10 ent) ent1))
		(entmake ent1)
		(setq count (1+ count))
	)
	(princ)
)