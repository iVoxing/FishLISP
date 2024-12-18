
(defun dim_uni (dim_ss / cl amnt idx ent horv pt1 pt2 x1 x2 y1 y2 x_list y_list pt0 lay)
	(setvar "cmdecho" 0)
	(setq cl (getvar "clayer"))
	(initget "Yes No")
	(setq kw (getkword "\nDelete old objects? <Y> "))
	(if kw nil (setq kw "Yes"))
	(setq 
		amnt (sslength dim_ss)
		idx 0
		x_list (list)
		y_list (list)
	)
	(repeat amnt
		(setq ent (entget (ssname dim_ss idx)))
		(if horv nil (setq horv (cdr (assoc 50 ent))))
		(if pt0 nil (setq pt0 (cdr (assoc 10 ent))))
		(if lay nil (setq lay (cdr (assoc 8 ent))))
		(if (= horv (cdr (assoc 50 ent)))
			 (setq 
			 	pt1 (cdr (assoc 13 ent))
				pt2 (cdr (assoc 14 ent))
				x1 (car pt1)
				y1 (cadr pt1)
				x2 (car pt2)
				y2 (cadr pt2)
				x_list (append x_list (list x1 x2))
				y_list (append y_list (list y1 y2))
			 )
		)
		(setq idx (1+ idx))
	)
	(setvar "clayer" lay)
	(cond 
		((= horv 0.0)
			(setq 
				x1 (apply 'min x_list)
				x2 (apply 'max x_list)
				pt1 (list x1 y1)
				pt2 (list x2 y1)
			)
			(cmd "dim1" "hor" pt1 pt2)
		)
		((= horv (/ pi 2))
			(setq
				y1 (apply 'min y_list)
				y2 (apply 'max y_list)
				pt1 (list x1 y1)
				pt2 (list x1 y2)
			)
			(cmd "dim1" "ver" pt1 pt2)
		)
		(t)
	)
	(if (= kw "Yes")
		 cmd pt0 "" "_erase" dim_ss "")
		 (progn
			(setvar "cmdecho" 1)
			(princ "Dimension line location: ")
			(cmd pause "")
		 )
	)
	(redraw)
	(setvar "cmdecho" 1)
)

(defun c:dimuni (/ ss)
	(prompt "\nSelect dimensions to edit: ")
	(setq ss (ssget '((0 . "dimension"))))
	(if ss (dim_uni ss))
	(princ)
)
	