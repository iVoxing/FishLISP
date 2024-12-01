; FishLISP ParkUtilities
; 2004-7-28 v1.0修改：C:WRID，增加序号检查。已经可用。
; 2004-7-27 v0.1增加：C:WRID，直接写按顺序增加的文字，以便处理老图及复杂图形。
; 2004-7-16 v0.0排序是个大问题
; 是否加入文字高度调整？

(defun c:pkrid (/ park_ss idx en att_ent park_amt)
	(if (setq park_ss (ssget "x" '((0 . "insert")(2 . "park*"))))
		(progn
		(setq idx 0)
		(repeat (setq park_amt (sslength park_ss))
			(setq 
				att_ent (entget (entnext (setq en (ssname park_ss idx))))
				att_ent (subst (cons 50 0) (assoc 50 att_ent) att_ent)
				att_ent (subst (cons 1 (rtos (- park_amt idx) 2 0)) (assoc 1 att_ent) att_ent)
			)
			(entmod att_ent)
			(entupd en)
			(setq idx (1+ idx))
		);repeat
		); progn park_ss
	); if park_ss?
	(princ (strcat "\n重新编号" (rtos park_amt 2 0) "个车位。"))
	(princ)
)

(defun c:wrid (/ id_ss id_cor id_amt idx id_fnd pt0 id_pt id_txt id_lay id_hig)
	(setq 
		id_ss (ssget "x" '((0 . "text")(8 . "park")))
		id_cor t
	)
	(if id_ss
		(progn
			(setq 
				id_amt (sslength id_ss)
				idx 1
			)
			(while (and id_cor (<= idx id_amt))
				(if (setq id_fnd (ssget "x" (list (cons 0 "text") (cons 8 "park") (cons 1 (rtos idx 2 0)))))
					(if (= (sslength id_fnd) 1)
						(setq idx (1+ idx)); id correct
						(progn
							(princ (strcat "\n编号" (rtos idx 2 0) "重复，请检查。"))
							(setq id_cor nil)
						);id exist but duplicate
					)
					(progn
						(princ (strcat "\n编号" (rtos idx 2 0) "遗漏，请检查。"))
						(setq id_cor nil)
					);progn id not exist
				);if 
			);while 
		);progn id_ss
		(setq id_amt 0)
	)
	(if id_cor
		(progn
			(setq 
				id (1+ id_amt)
				id_pt (getpoint "\n编号点：")
			)
			(while id_pt
				(setq 
					pt0 id_pt
					id_pt (trans id_pt 1 0)
					id_txt (rtos id 2 0)
					id_lay "park"
					id_hig (* 4.5 (getvar "dimscale"))
				)
				(entmake (list '(0 . "text") (cons 8 "park") (cons 10 id_pt) (cons 11 id_pt) (cons 1 id_txt) (cons 40 id_hig) (cons 72 1) (cons 73 2)))
				(setq 
					id (1+ id)
					id_pt (getpoint pt0 "\n编号点：")
				)
			);while id_pt
		);progn id_cor
	);if id_cor?
	(princ)
)

