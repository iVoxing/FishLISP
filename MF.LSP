; Á¬ÐøµÄFILLET

(defun c:mf (/ fr ent en en1 lkd olderr)
	(setq olderr *error*)
	(defun *error* (s)
		(if en1 (redraw (car en1) 4))
		(setq en1 nil)
		(setq *error* olderr olderr nil)
		(princ)
	)

	(princ "\nFishLISP. 11-30-1998 V2.0. ")
	(setvar "cmdecho" 0)
	(setq en1 nil)
	(while (setq en (entsel "\nPick line to edit: "))
		(if
			(and
				(or
					(= (cdr (assoc 0 (setq ent (entget (car en))))) "LINE")
					(= (cdr (assoc 0 ent)) "ARC")
				)
				(/= (setq lkd (cdr (assoc 70 (tblsearch "layer" (cdr (assoc 8 ent)))))) 68)
			)
			(progn
				(if en1 (cmd "_fillet" en1 en))
				(redraw (car en) 3)
				(setq en1 en)
			)
			(progn
				(if (= lkd 68) (princ "\n1 was on a locked layer. ") (princ "\n1 was not LINE or ARC. "))
				(princ "1 was filtered out. ")
			)
		)
	)
	(if en1 (redraw (car en1) 4))
	(setvar "cmdecho" 1)
	(princ)
)

(princ "loaded. Start as C:MF ")
(princ)