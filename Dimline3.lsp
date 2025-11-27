; FISHLISP
; C:AIDD
; 根据轴线，自动添加轴线号。
; History:
; 2005-09-28 v0.9 
;

(defun c:dl ()
	;<<用户图层设置
	(setq id_lay FLLT_AXIS
				lay FLLT_AXIS
	)
	;用户图层设置>>
	(setq 
		sca (getvar "dimscale")
		rad (* sca 4.0)
		cu_lay (getvar "clayer")
	)
	(setvar "cmdecho" 0)
	(setvar "clayer" id_lay)
	(if (setq line_ss (ssget '((0 . "LINE"))))
		(progn
			(setq 
				x_list (list)
				y_list (list)
				xpt1_list (list)
				xpt2_list (list)
				ypt1_list (list)
				ypt2_list (list)
				ss_idx 0
			)
			(repeat (sslength line_ss)
				(setq 
					line_ent (entget (ssname line_ss ss_idx))
					line_pt1 (trans (cdr (assoc 10 line_ent)) 0 1)
					line_pt2 (trans (cdr (assoc 11 line_ent)) 0 1)
				)
				(cond
					((equal (setq ptxy (car line_pt1)) (car line_pt2) 0.00001)
						(if (member ptxy x_list)
							nil
							(setq 
								xpt1_list (append xpt1_list (list line_pt1))
								xpt2_list (append xpt2_list (list line_pt2))
								x_list (append x_list (list ptxy))
							)
						)
					)
					((equal (setq ptxy (cadr line_pt1)) (cadr line_pt2) 0.00001)
						(if (member ptxy y_list)
							nil
							(setq 
								ypt1_list (append ypt1_list (list line_pt1))
								ypt2_list (append ypt2_list (list line_pt2))
								y_list (append y_list (list ptxy))
							)
						)
					)
					( t 
					)
				)
				(setq ss_idx (1+ ss_idx))
			)
			(setq 
				line_ss nil
				xpt1_list (vl-sort xpt1_list '(lambda (pt1_ pt2_) (< (car pt1_) (car pt2_))))
				xpt2_list (vl-sort xpt2_list '(lambda (pt1_ pt2_) (< (car pt1_) (car pt2_))))
				ypt1_list (vl-sort ypt1_list '(lambda (pt1_ pt2_) (< (cadr pt1_) (cadr pt2_))))
				ypt2_list (vl-sort ypt2_list '(lambda (pt1_ pt2_) (< (cadr pt1_) (cadr pt2_))))
				id_x_list (mapcar '(lambda (x_) (rtos (1+ (vl-position x_ xpt1_list)) 2 0)) xpt1_list)
			)
			(mapcar
				'(lambda (pt1_ pt2_ ins_at_)
					(setq ins_bk (if (> (strlen ins_at_) 1) "axi" "axi0"))
					(if (> (cadr pt1_) (cadr pt2_))
						(setq temp pt1_ pt1_ pt2_ pt2_ temp temp nil)
					)
					(setq pt1_ (subst (- (cadr pt1_) rad) (cadr pt1_) pt1_))
					(setq pt2_ (subst (+ (cadr pt2_) rad) (cadr pt2_) pt2_))
					(cmd "_insert" ins_bk pt1_ sca "" 0 ins_at_)
					(cmd "_insert" ins_bk pt2_ sca "" 0 ins_at_)
				)
				xpt1_list
				xpt2_list
				id_x_list
			)
			(mapcar
				'(lambda (pt1_ pt2_ ins_at_)
					(setq ins_bk (if (> (strlen ins_at_) 1) "axi" "axi0"))
					(if (> (car pt1_) (car pt2_))
						(setq temp pt1_ pt1_ pt2_ pt2_ temp temp nil)
					)
					(setq pt1_ (subst (- (car pt1_) rad) (car pt1_) pt1_))
					(setq pt2_ (subst (+ (car pt2_) rad) (car pt2_) pt2_))
					(cmd "_insert" ins_bk pt1_ sca "" 0 ins_at_)
					(cmd "_insert" ins_bk pt2_ sca "" 0 ins_at_)
				)
				ypt1_list
				ypt2_list
				id_y_list
			)
		)
	)
	(setvar "cmdecho" 1)
	(setvar "clayer" cu_lay)
	(princ)
)
(princ "FishLISP C:AIDD ")
(princ)