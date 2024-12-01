; FISHLISP
; C:AW
; Draw an array of rectangles inside one.
; History:
;	2020-11-09 v3.0 NEW:		New method, NOT READY
; 	2003-09-04 v2.1	IMPROVE:	More convenient option.
; 					NEW:		Continuous mode ON/OFF.
; 	2003-09-03 v2.0 NEW:		2 dimension array.
; 					IMPROVE:	Drawing method.
; 	2002-07-25 v1.1 FIXED:		Unexpected result if 1st point is not lower-left point.
; 	2002-06-18 v1.0 Original	1 dimension array only.

(defun c:aw (/ olderr cg_wid cg_x_a cg_y_a pt1 pt2 n m x_wid y_wid cg_hfw x1 x2 x3 x4 y1 y2 y3 y4 os list_x list_y plwd)
	(setq olderr *error*)
	(defun *error* (s)
		;(alert s)
		(if os (setvar "osmode" os))
		(if plwd (setvar "plinewid" plwd))
		(fl_undo_end)
		(setq *error* olderr)
		(princ)
	)
	(fl_undo_begin)
	(princ "\nFISHLISP C:AW v3.0.")
	(if aw_cg_wid nil (setq aw_cg_wid 50.0))
	(if aw_cg_x_a nil (setq aw_cg_x_a 2))
	(if aw_cg_y_a nil (setq aw_cg_y_a 2))
	(aw_opt)
	(setq cg_wid aw_cg_wid)
	(setq cg_x_a aw_cg_x_a)
	(setq cg_y_a aw_cg_y_a)
	(fl_undo_end)
	(princ)
);defun c:aw

(defun get_rct_list (pt1 pt2)
	(setq
		rct_list (list)
		h_n = 1
		v_n = 1
	)
	;; 这样好像也没啥区别。。。
	(repeat v_amt
		(repeat h_amt
			(setq
				o1x = (car pt1)	o1y = (cadr pt1)
				o2x = (car pt2)	o2y = (cadr pt2)
				rct_width 	= (/ (abs (- o1x o2x)) h_n)
				rct_height	= (/ (abs (- o1y o2y)) v_n)
				x_ad = (+ o1x w (* (- h_n 1) (+ w rct_width)))
				x_bc = (+ o1x (* h_n (+ w rct_width)))
				y_ab = (+ o1y w (* (- v_n 1) (+ w rct_height)))
				y_cd = (+ o1y (* v_n (+ w rct_height)))
				pt_a = (list x_ad y_ab 0.0); force WCS
				pt_b = (list x_bc y_ab 0,0)
				pt_c = (list x_bc y_cd 0,0)
				pt_d = (list x_ad y_cd 0,0)
			)
			(setq h_n = (1+ h_n))
		)
		(setq v_n = (1+ v_n))
	)
	(append rct_list (list (list pt_a pt_b pt_c pt_d)))
	rct_list
)

(defun draw_rct ()
	(foreach rct rct_list
		pl_by_emk (rct)
	)
)

; command, rct as 4 points list, 
(defun pl_by_cmd (rct)
	(foreach var '("cmdecho" "osmode" "plinewid")
		(setvar var 0)
	)
	(cmd "pline")
	(apply 'cmd rct)
	(cmd "c")
)

; entmake
(defun pl_by_emk (rct)
	; group code 70: 1 = close, 0 = open
	(entmake '((0 . "POLYLINE") (66 . 1) (70 . 1)))
	(foreach pt rct
		(entmake (list (cons 0 "VERTEX") (cons 10 pt))); NOTE: vtx made in WCS!
	)
	(entmake '((0 . "SEQEND")))
)

(defun aw_opt ()
	(princ "\n选项:[格间宽度 W：")
	(princ aw_cg_wid)
	(princ "，横向分格 X：")
	(princ aw_cg_x_a)
	(princ "，纵向分格 Y：")
	(princ aw_cg_y_a)
	(princ "，连续模式 C：")
	(princ (if aw_cnt "ON]" "OFF]"))
	(initget "W X Y C")
	(setq pt1 (getpoint "\n[W X Y C 更改选项]矩形角点："))
	(cond 
		((= pt1 "W")
			(princ "\n格间宽度 W：<")
			(princ aw_cg_wid)
			(princ "> ")
			(setq cg_wid (getreal))
			(setq cg_wid (if cg_wid cg_wid aw_cg_wid))
			(setq aw_cg_wid cg_wid)
			(aw_opt)
		)
		((= pt1 "X")
			(princ "\n横向分格 X：<")
			(princ aw_cg_x_a)
			(princ "> ")
			(setq cg_x_a (getint))
			(setq cg_x_a (if cg_x_a cg_x_a aw_cg_x_a))
			(setq aw_cg_x_a cg_x_a)
			(aw_opt)
		)
		((= pt1 "Y")
			(princ "\n纵向分格 Y：<")
			(princ aw_cg_y_a)
			(princ "> ")
			(setq cg_y_a (getint))
			(setq cg_y_a (if cg_y_a cg_y_a aw_cg_y_a))
			(setq aw_cg_y_a cg_y_a)
			(aw_opt)
		)
		((= pt1 "C")
			(setq aw_cnt (not aw_cnt))
			(aw_opt)
		)
		( t
		)
	);cond
)

(princ "loaded. Start as C:AW ")
(princ)

