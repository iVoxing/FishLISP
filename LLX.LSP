; 交叉连接两条LINE的端点

(defun make_line (pt_start pt_end)
	(entmake (list '(0 . "line") (cons 10 pt_start) (cons 11 pt_end)))
)

(defun c:llx (/ olderr l1 l2 ent1 ent2 pt1-1 pt1-2 pt2-1 pt2-2) 
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
			pt1-1 (cdr (assoc 10 ent1))
			pt1-2 (cdr (assoc 11 ent1))
			pt2-1 (cdr (assoc 10 ent2))
			pt2-2 (cdr (assoc 11 ent2))
		)
		(if (inters pt1-1 pt2-1 pt1-2 pt2-2) 
			(mapcar 'make_line (list pt1-1 pt1-2) (list pt2-1 pt2-2))
			(mapcar 'make_line (list pt1-1 pt1-2) (list pt2-2 pt2-1))
		)
		)
	)
	(setq *error* olderr)
	(princ)
)

(princ "loaded. Start as C:LLX ")
(princ)