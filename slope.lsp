; 车库坡度基本计算
; 通过起止点计算是否满足最大坡度限制，并绘制最大坡度或合适坡度的关键剖线

(defun c:fslope (/ osm pt1 pt2 h0 b0 slp1 b_min h1)
	;; 基本设置
	(setq osm (getvar "osmode"))
	;; 获取起止点
	(setq pt1 (getpoint "\n起点："))
	(if pt1 (setq pt2 (getpoint pt1 "\n终点")))
	(if pt2
		(if (> (car pt1) (car pt2))
			(setq temp pt1 pt1 pt2 pt2 temp temp nil)
		)
	)
	;; 根据高差计算最小坡长，以确定起止点是否满足最大坡度限制
	(setq 
		h0 (- (cadr pt2) (cadr pt1))
		b0 (- (car pt2) (car pt1))
	)
	(setq slp1 (getreal "\n缓坡水平长度<3600>："))
	(setq slp1 (if slp1 slp1 3600.0))
	(setq b_min (+ (* slp1 2) (/ (- h0 (* slp1 2 0.075)) 0.15)))
	;; 
	(setvar "osmode" 0)
	(cond
		((< b0 b_min) ; 不满足限制
			(alert "起止点不满足最大坡度限制\n绘制最大坡度作为参考")
			(setq 
				spt1 pt2
				spt2 (list (- (car pt2) slp1) (- (cadr pt2) (* slp1 0.075)))
				spt3 (list (- (car spt2) (/ (- h0 (* slp1 0.15)) 0.15)) (+ (cadr pt1) (* slp1 0.075)))
				spt4 (list (- (car spt3) slp1) (cadr pt1))
			)
		);< b0 b_min
		(t
			(princ "\n起止点满足最大坡度限制。")
			(setq 
				h1 (/ (* (/ h0 2) slp1) (- b0 slp1))
				spt1 pt2
				spt2 (list (- (car pt2) slp1) (- (cadr pt2) h1))
				spt3 (list (+ (car pt1) slp1) (+ (cadr pt1) h1))
				spt4 pt1
			)
		);>= b0 b_min
	);cond
	(setvar "cmdecho" 0)
	(cmd "line" spt1 spt2 spt3 spt4 "")
	(setvar "cmdecho" 1)
	;; 恢复基本设置
	(setvar "osmode" (if osm osm 175))
);defun

(princ " Start as C:FSLOPE")