; 2025-11-16	rewrite, tested ok

(defun c:arr (/ arr_ent)
	(prompt "\n[FishLISP] Draw arrow line. C:ARR ")
	(setq loop t arr_ent nil)
	(setq pt0 (getpoint "\n起点："))
	(if pt0 nil (exit))
	(while loop
		(setq pt1 (getpoint pt0 "\n下一点："))
		(if pt1
			(progn
				(make_arr pt0 pt1)
				(setq pt0 pt1)
			)
			(setq loop nil)
		)
	)
	(princ)
)

(defun make_arr (pt_0 pt_1)
	(if arr_ent
		(progn
			(setq arr_ent (append arr_ent (list (cons 10 pt_1))))
			(entmod arr_ent)
		); following point
		(progn
			(setq arr_ent
				(list	
					'(0 . "LEADER")
					(cons 3 (strcat (getvar "dimstyle") "$7"))
					(cons 8 FLLT_DIM0)
					'(71 . 1)
					'(72 . 0)
					'(73 . 3)
					'(74 . 1)
					'(75 . 0)
					'(100 . "AcDbEntity")
					'(100 . "AcDbLeader")
					; points must be placed here.
					(cons 10 pt_0)
					(cons 10 pt_1)
				)
			)
			(entmake arr_ent)
			(setq arr_ent (entget (entlast)))
		); first point
	)
)

(princ "FishLISP C:ARR ")
(princ)
