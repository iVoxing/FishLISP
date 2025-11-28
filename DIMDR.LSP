; 标注门的尺寸
; 2025-11-28	rewrited, test ok
;

(defun dr_dim (ss_ / dsc idx ent cod ins scl ang rot tpt)
	(setq dsc (getvar "dimscale"))
	(setq idx 0)
	(repeat (sslength ss_)
		(setq
			ent (entget (ssname ss_ idx))
			cod (substr (cdr (assoc 2 ent)) 4 1)
			ins (cdr (assoc 10 ent))
			scl (abs (cdr (assoc 41 ent)))
			ang (cdr (assoc 50 ent))
			rot (if (member cod (list "2" "5" "6" "8")) (+ ang (/ pi 2.0)) (- ang (/ pi 2.0)))
			tpt	(polar ins rot (* 5.0 dsc))
			idx (1+ idx)
		)
		;(getpoint ins "\ntest")
		(princ "\ntest ")
		(princ cod)
		(entmake 
			(list
				'(0 . "TEXT") 
				'(7 . "STANDARD")
				;水平居中
				'(72 . 1)
				;垂直居中 
				'(73 . 2) 
				(cons 1 (strcat "M-" (rtos scl 2 0))) 
				(cons 8 FLLT_DIM0) 
				(cons 10 tpt) 
				(cons 11 tpt) 
				(cons 40 (* dsc 3.0))
				(cons 50 (+ ang pi))
			)
		)
	)
)

(defun c:dimdr (/ key ss flt)
	(setq flt '((0 . "INSERT")(2 . "dr0?")))
	(initget "All")
	(setq key (getkword "\n[All]: <select>"))
	(if (= key "All")
		 (setq ss (ssget "x" flt))
		 (setq ss (ssget flt))
	)
	(if ss (dr_dim ss))
	(princ)
)
