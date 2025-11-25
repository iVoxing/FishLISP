; FISHLISP
; C:AW
; Draw an array of rectangles inside one.
; History:
;	2025-11-25					rewrited, tested ok
;	2020-11-09 v3.0 NEW:		New method, NOT READY
; 	2003-09-04 v2.1	IMPROVE:	More convenient option.
; 					NEW:		Continuous mode ON/OFF.
; 	2003-09-03 v2.0 NEW:		2 dimension array.
; 					IMPROVE:	Drawing method.
; 	2002-07-25 v1.1 FIXED:		Unexpected result if 1st point is not lower-left point.
; 	2002-06-18 v1.0 Original	1 dimension array only.

(defun c:aw (/ PT1 pt2)
	(setq *error* *pub_err*)
	(setq AW_CG_WID (if AW_CG_WID AW_CG_WID 50.0))
	(setq AW_CG_X_A (if AW_CG_X_A AW_CG_X_A 2))
	(setq AW_CG_Y_A (if AW_CG_Y_A AW_CG_Y_A 2))
	(aw_opt)
	(while PT1
		(setq pt2 (getpoint PT1 "\n矩形对角点："))
		(if pt2
			(draw_rects PT1 pt2)
		)
		(setq PT1 (if AW_CONTINUE (getpoint "\n矩形角点：") nil))
	)
	(princ)
)

(defun draw_rects (pt1_ pt2_ / v_n h_n o1x o1y o2x o2y rct_wid rct_hei rx_min rx_max ry_min ry_max pt_a pt_b pt_c pt_d)
	(setq v_n 1)
	(while (<= v_n AW_CG_Y_A)
		(setq h_n 1)
		(while (<= h_n AW_CG_X_A)
			(setq
				o1x (apply 'min (list (car pt1_) (car pt2_)))
				o1y (apply 'min (list (cadr pt1_) (cadr pt2_)))
				o2x (apply 'max (list (car pt1_) (car pt2_)))
				o2y (apply 'max (list (cadr pt1_) (cadr pt2_)))

				rct_wid (- (/ (- o2x o1x AW_CG_WID) AW_CG_X_A) AW_CG_WID)
				rct_hei	(- (/ (- o2y o1y AW_CG_WID) AW_CG_Y_A) AW_CG_WID)

				rx_min (+ o1x AW_CG_WID (* (1- h_n) (+ rct_wid AW_CG_WID)))
				ry_min (+ o1y AW_CG_WID (* (1- v_n) (+ rct_hei AW_CG_WID)))
				rx_max (+ o1x (* h_n (+ rct_wid AW_CG_WID)))
				ry_max (+ o1y (* v_n (+ rct_hei AW_CG_WID)))

				pt_a (list rx_min ry_min 0.0)
				pt_b (list rx_max ry_min 0.0)
				pt_c (list rx_max ry_max 0.0)
				pt_d (list rx_min ry_max 0.0)
			)

			(fl_make_pline (list pt_a pt_b pt_c pt_d) 1)
			(setq h_n (1+ h_n))
		)
		(setq v_n (1+ v_n))
	)
)

(defun aw_opt (/ cg_wid cg_x_a cg_y_a)
	(princ 
		(strcat 
			"\n选项:[格间宽度 W："
			(rtos AW_CG_WID)
			"，横向分格 X："
			(rtos AW_CG_X_A)
			"，纵向分格 Y："
			(rtos AW_CG_Y_A)
			"，连续模式 C："
			(if AW_CONTINUE "ON]" "OFF]")
		)
	)
	(initget "W X Y C")
	(setq PT1 (getpoint "\n[W X Y C 更改选项]矩形角点："))
	(cond 
		((= PT1 "W")
			(setq cg_wid (getreal (strcat "\n格间宽度 W：<" (rtos AW_CG_WID) ">")))
			(setq cg_wid (if cg_wid cg_wid AW_CG_WID))
			(setq AW_CG_WID cg_wid)
			(aw_opt)
		)
		((= PT1 "X")
			(setq cg_x_a (getint (strcat "\n横向分格 X：<" (rtos AW_CG_X_A) ">")))
			(setq cg_x_a (if cg_x_a cg_x_a AW_CG_X_A))
			(setq AW_CG_X_A cg_x_a)
			(aw_opt)
		)
		((= PT1 "Y")
			(setq cg_y_a (getint (strcat "\n纵向分格 Y：<" (rtos AW_CG_Y_A) ">")))
			(setq cg_y_a (if cg_y_a cg_y_a AW_CG_Y_A))
			(setq AW_CG_Y_A cg_y_a)
			(aw_opt)
		)
		((= PT1 "C")
			(setq AW_CONTINUE (not AW_CONTINUE))
			(aw_opt)
		)
		(t)
	)
)

(princ "loaded. Start as C:AW ")
(princ)

