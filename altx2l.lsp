; FISHLISP
; C:ATL
; Align Text to Line object
; History:
;	2025-11-15	rewrite function, tested ok
; 	2005-9-26 v1.0 

(defun c:atl (/ l_obj l_ent l_pt1 l_pt2 l_ang t_ang t_obj t_ent)
	(prompt "\n[FishLISP] Align text to line. C:ATL ")
	(setq l_obj (entsel "\nSelect Line: "))
	(setq loop t)
	(if l_obj
		(while loop
			(setq t_obj (entsel "\nSelec Text: "))
			(if t_obj
				(align_txt_2_line t_obj l_obj)
				(setq loop nil)
			)
		)
	)
	(princ)
)

(defun align_txt_2_line (text_obj line_obj)
	(setq 
		l_ent (entget (car line_obj))
		l_ang (angle (cdr (assoc 10 l_ent)) (cdr (assoc 11 l_ent)))
		t_ent (entget (car text_obj))
		t_ang (cdr (assoc 50 t_ent))
	)
	(if t_ang
		(entmod (subst (cons 50 l_ang) (assoc 50 t_ent) t_ent))
	)
)

(princ " FishLISP C:ATL ")
(princ)