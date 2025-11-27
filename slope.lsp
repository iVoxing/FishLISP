; 车库坡度基本计算
; 通过起止点计算是否满足最大坡度限制，并绘制最大坡度或合适坡度的关键剖线
; 2025-11-16	rewrited, 
;				一个思路：可以用UCS，但本就不复杂，所以也简化不了啥

(setq DRAW_REF t)

(defun c:fsl (/ osm pt1 pt2)
	;; 获取起止点
	(setq pt1 (getpoint "\n起点："))
	(if pt1 (setq pt2 (getpoint pt1 "\n终点")))
	(if pt2
		(progn
			(setq flat_w (getreal "\n缓坡水平长度: <3600>"))
			(setq flat_w (if flat_w flat_w 3600.0))
		)
	)
	(if (is_slope_ok pt1 pt2 flat_w)
		(draw_slope pt1 pt2 flat_w)
		(alert_min)
	)
	(princ)
)

(defun is_slope_ok (pt1_ pt2_ flat_wid_)
	(setq 	x_dif (abs (- (car pt1_) (car pt2_)))
			y_dif (abs (- (cadr pt1_) (cadr pt2_)))
			x_min (+ (* flat_wid_ 2) (/ (abs (- y_dif (* flat_wid_ 0.15))) 0.15))
	)
	(>= x_dif x_min)
)

(defun alert_min (/ msg adj)
	(setq msg
		(strcat
			"起止点不满足最大坡度限制"
			"\n坡道投影长度为："
			(rtos x_dif)
			", 最小应为："
			(rtos x_min)
		)
	)
	(alert msg)
	(if DRAW_REF
		(progn
			(setq adj (if (> (car pt2) (car pt1)) x_min (- x_min)))
			(setq pt2 (list (+ (car pt1) adj) (cadr pt2)))
			(draw_slope pt1 pt2 flat_w)
		)
	)
)

(defun draw_slope (pt1_ pt2_ flat_wid_ / flat_hei spt1 spt2)
	(setq 	x_dif (abs (- (car pt1_) (car pt2_)))
			y_dif (abs (- (cadr pt1_) (cadr pt2_)))
			x_min (+ (* flat_wid_ 2) (/ (abs (- y_dif (* flat_wid_ 0.15))) 0.15))
	)
	(setq flat_hei (/ (* (/ y_dif 2) flat_wid_) (- x_dif flat_wid_)))
	(princ "\ntest: ")
	(princ flat_hei)
	(setq flat_wid_ 
		(if (> (car pt2_) (car pt1_))
			flat_wid_
			(- flat_wid_)
		)
	)
	(setq flat_hei 
		(if (> (cadr pt2_) (cadr pt1_))
			flat_hei 
			(- flat_hei)
		)
	)
	(setq 	spt1 (list (+ (car pt1_) flat_wid_) (+ (cadr pt1_) flat_hei))
			spt2 (list (- (car pt2_) flat_wid_) (- (cadr pt2_) flat_hei))
	)
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(setvar "cmdecho" 0)
	(cmd "_line" pt1_ spt1 spt2 pt2_ "")
	(setvar "cmdecho" 1)
	(setvar "osmode" os)
)

(princ " Start as C:FSL")