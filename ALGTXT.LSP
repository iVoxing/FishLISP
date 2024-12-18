(defun c:at (/ olderr ss pt0 pt1 x0 y0 x1 y1 amt en1 ent1)
	(setq olderr *error*)
	(defun *error* (s)
		(setvar "cmdecho" 0)
		(fl_undo_end)
		(setq *error* olderr olderr nil)
		(princ)
	)
	(setvar "cmdecho" 0)

	(defun getpt (/ pt)
		(initget "Horizontally Vertically")
		(princ (strcat "\nDirection: " (if algtxt_k "Horizontally" "Vertically")))
		(setq pt (getpoint "\n[Horizontally/Vertically]<pick a point>: "))
		(cond
			((= pt "Horizontally")
				(setq algtxt_k t)
				(getpt)
			)
			((= pt "Vertically")
				(setq algtxt_k nil)
				(getpt)
			)
			((listp pt)
				pt
			)
			( t 
				(setq pt nil)
			)
		);pt
	)

	(fl_undo_begin)
	(princ "\nSelect text objects: ")
	(setq ss (ssget '((0 . "text"))))
	(if ss (setq pt0 (getpt)))
	(if pt0
		(setq
			x0 (car pt0)
			y0 (cadr pt0)
			amt (sslength ss)
			idx 0
		)
	)
	(repeat amt
		(setq 
			en1 	(ssname ss idx)
			ent1	(entget en1)
			pt1		(cdr (assoc 10 ent1))
			x1		(car pt1)
			y1		(cadr pt1)
			pt1		(if algtxt_k (subst y0 y1 pt1) (subst x0 x1 pt1))
			ent1	(subst (cons 10 pt1) (assoc 10 ent1) ent1)
		)
		(entmod ent1)
		(setq idx (1+ idx))
	);repeat
	(fl_undo_end)
	(setq *error* olderr)
	(princ)
)
