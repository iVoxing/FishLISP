; 交叉连接两条LINE的端点

(defun make_line (pt1_ pt2_)
	(entmake (list '(0 . "LINE") (cons 10 pt1_) (cons 11 pt2_)))
)

(defun c:llx (/ olderr l1 l2 ent1 ent2 pt1_1 pt1_2 pt2_1 pt2_2) 
	(setq olderr *error*)
	(defun *error* (s) 
		(setq *error* (if olderr olderr *pub_err*))
		(princ)
	)
	(setq l1 (entsel "\nSelect line: "))
	(if 
		(and l1 
			(= (cdr (assoc 0 (entget (car l1)))) "LINE")
		)
		(setq l2 (entsel "\nSelect line: "))
	)
	(if 
		(and l2 
			(= (cdr (assoc 0 (entget (car l2)))) "LINE")
		)
		(progn 
		(setq 
			ent1 (entget (car l1))
			ent2 (entget (car l2))
			pt1_1 (cdr (assoc 10 ent1))
			pt1_2 (cdr (assoc 11 ent1))
			pt2_1 (cdr (assoc 10 ent2))
			pt2_2 (cdr (assoc 11 ent2))
		)
		(if (inters pt1_1 pt2_1 pt1_2 pt2_2) 
			(mapcar 'make_line (list pt1_1 pt1_2) (list pt2_1 pt2_2))
			(mapcar 'make_line (list pt1_1 pt1_2) (list pt2_2 pt2_1))
		)
		)
	)
	(setq *error* olderr)
	(princ)
)

(princ "loaded. Start as C:LLX ")
(princ)