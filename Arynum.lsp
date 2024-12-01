; This LISP application is a good sample of
; function "subst" "cons" and "entmake"
;
;* C:Arynum	* Array nummber text
(defun c:arynum (/ ent txt0 num0 pt0 x0 y0 ay_dir ay_num ay_dis nm_add count x1 y1 num1 txt1 pt1 ent1)
	(princ "\nArray Nummber Text. FishLISP. Dec 14 1995")
	; Select the base nummber text to array
	(prompt "\nSelect the base nummber text to edit:")
	(setq ent (entget (car (entsel))))
	(while (/= (cdr (assoc 0 ent)) "TEXT")
		 (progn
			 (princ "\nThe selection is not TEXT. Select again:")
			 (setq ent (entget (car (entsel))))
		 )
	) 
	(setq 
		txt0 (cdr (assoc 1 ent))
		num0 (read txt0)
		pt0 (trans (cdr (assoc 10 ent)) 0 1)
		x0 (car pt0) y0 (cadr pt0)
	)
 
	(setq ay_dir (getstring "\nArray direction--X or <Y>:"))
	(if (= ay_dir "") (setq ay_dir "y"))
	(setq ay_num (getint "\nAount to array:"))
	(setq ay_dis (getdist "\nArray distance:"))
	(setq nm_add (getreal "\nNummber add each:"))

	(setq count 1)
	(while (/= count ay_num)
		(if (= ay_dir "y")
			 (setq 
			 	x1 x0
				y1 (+ (* ay_dis count) y0)
			 )
			 (setq 
			 	x1 (+ (* ay_dis count) x0)
				y1 y0
			 )
		)
		(setq 
			num1 (+ (* nm_add count) num0)
			txt1 (rtos num1 2 0)
			pt1 (trans (list x1 y1) 1 0)
			;pt1 (list x1 y1)
		)
		(setq ent1 (subst (cons 1 txt1) (assoc 1 ent) ent))
		(setq ent1 (subst (cons 10 pt1) (assoc 10 ent) ent1))
		(entmake ent1)
		(setq count (1+ count))
	)
	(princ)
)