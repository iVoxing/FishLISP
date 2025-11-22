; FISHLISP
; C:AW
; Draw an array of rectangles inside one.
; History:
;	2025-11-21		optimization
; 	2003-09-04 v2.1	IMPROVE:	More convenient option.
; 					NEW:		Continuous mode ON/OFF.
; 	2003-09-03 v2.0 NEW:		2 dimension array.
; 					IMPROVE:	Drawing method.
; 	2002-07-25 v1.1 FIXED:		Unexpected result if 1st point is not lower-left point.
; 	2002-06-18 v1.0 Original	1 dimension array only.

(defun err (s_)
	(if plwd (setvar "plinewid" plwd))
	(*error* s_)
	(princ)
)

(setq *error* err)

(defun c:aw (/ olderr cg_wid cg_x_a cg_y_a pt1 pt2 n m x_wid y_wid cg_hfw x1 x2 x3 x4 y1 y2 y3 y4 os list_x list_y plwd)

	(princ "\nFISHLISP C:AW v2.1.")
	(setq aw_cg_wid (if aw_cg_wid aw_cg_wid 50.0))
	(setq aw_cg_x_a (if aw_cg_x_a aw_cg_x_a 2))
	(setq aw_cg_y_a (if aw_cg_y_a aw_cg_y_a 2))
	(aw_opt)
	(setq cg_wid aw_cg_wid)
	(setq cg_x_a aw_cg_x_a)
	(setq cg_y_a aw_cg_y_a)
	(while pt1
		(setq pt2 (getpoint pt1 "\n矩形对角点："))
		(if pt2
			(progn
				(setq os (getvar "osmode") plwd (getvar "plinewid"))
				(setvar "cmdecho" 0)
				(setvar "osmode" 0)
				(setvar "plinewid" 0)
				(setq 
					x_wid 	(/ (abs (- (abs (- (car pt2) (car pt1))) cg_wid)) cg_x_a)
					y_wid 	(/ (abs (- (abs (- (cadr pt2) (cadr pt1))) cg_wid)) cg_y_a)
					cg_hfw	(/ cg_wid 2.0)
				)
				(setq 
					list_x (list (car pt1) (car pt2))
					list_y (list (cadr pt1) (cadr pt2))
				)
				(setq 
					x1 (+ (apply 'min list_x) cg_hfw)
					y1 (+ (apply 'min list_y) cg_hfw)
					x2 (- (apply 'max list_x) cg_hfw)
					y2 y1
					x3 x2
					y3 (- (apply 'max list_y) cg_hfw)
					x4 x1
					y4 y3
				)
				(setq y_n 0 y_m 1)
				(repeat cg_y_a
					(setq x_n 0 x_m 1)
					(repeat cg_x_a
						(cmd "pline"
							(list (+ x1 (* x_wid x_n) cg_hfw) (+ y1 (* y_wid y_n) cg_hfw))
							(list (+ x1 (* x_wid x_m) (- cg_hfw)) (+ y1 (* y_wid y_n) cg_hfw))
							(list (+ x1 (* x_wid x_m) (- cg_hfw)) (+ y1 (* y_wid y_m) (- cg_hfw)))
							(list (+ x1 (* x_wid x_n) cg_hfw) (+ y1 (* y_wid y_m) (- cg_hfw)))
							"c"
						)
						(setq x_n (1+ x_n) x_m (1+ x_m))
					)
					(setq y_n (1+ y_n) y_m (1+ y_m))
				)
				(setvar "cmdecho" 1)
				(setvar "osmode" os)
				(setvar "plinewid" plwd)
			)
		)
		(setq pt1 (if aw_cnt (getpoint "\n矩形角点：") nil))
	)
	(princ)
)

(defun aw_opt ()
	(princ 
		(strcat 
			"\n选项:[格间宽度 W："
			(rtos aw_cg_wid)
			"，横向分格 X："
			(rtos aw_cg_x_a)
			"，纵向分格 Y："
			(rtos aw_cg_y_a)
			"，连续模式 C："
			(if aw_cnt "ON]" "OFF]")
		)
	)
	(initget "W X Y C")
	(setq pt1 (getpoint "\n[W X Y C 更改选项]矩形角点："))
	(cond 
		((= pt1 "W")
			(setq cg_wid (getreal (strcat "\n格间宽度 W：<" aw_cg_wid ">")))
			(setq cg_wid (if cg_wid cg_wid aw_cg_wid))
			(setq aw_cg_wid cg_wid)
			(aw_opt)
		)
		((= pt1 "X")
			(setq cg_x_a (getint (strcat "\n横向分格 X：<" aw_cg_x_a ">")))
			(setq cg_x_a (if cg_x_a cg_x_a aw_cg_x_a))
			(setq aw_cg_x_a cg_x_a)
			(aw_opt)
		)
		((= pt1 "Y")
			(setq cg_y_a (getint (strcat "\n纵向分格 Y：<" aw_cg_y_a ">")))
			(setq cg_y_a (if cg_y_a cg_y_a aw_cg_y_a))
			(setq aw_cg_y_a cg_y_a)
			(aw_opt)
		)
		((= pt1 "C")
			(setq aw_cnt (not aw_cnt))
			(aw_opt)
		)
		(t)
	)
)

(princ "loaded. Start as C:AW ")
(princ)

