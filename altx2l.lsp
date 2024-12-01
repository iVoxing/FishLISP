; FISHLISP
; C:ATL
; Align Text to Line object
; History:
; 	2005-9-26 v1.0 

(defun c:atl (/ l_obj l_ent l_pt1 l_pt2 l_ang t_ang t_obj t_ent)
	(if (setq l_obj (entsel "\n—°‘ÒLine£∫"))
		(progn
			(setq 
				l_ent (entget (car l_obj))
				l_pt1 (cdr (assoc 10 l_ent))
				l_pt2 (cdr (assoc 11 l_ent))
				l_ang (angle l_pt1 l_pt2)
				t_ang t
			)
			(while t_ang
				(if (setq t_obj (entsel "\n—°‘ÒText£∫"))
					(setq 
						t_ent (entget (car t_obj))
						t_ang (cdr (assoc 50 t_ent))
					)
					(setq t_ang nil)
				)
				(if t_ang
					(entmod (subst (cons 50 l_ang) (assoc 50 t_ent) t_ent))
				)
			);while
		);progn
	);if
	(princ)
)

(princ " FishLISP C:ATL ")
(princ)