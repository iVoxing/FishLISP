; FISHLISP
; C:AIDD
; 根据轴线，自动添加轴线号。
; History:
; 2025-11-24	重写轴号序列，已经扩展至足够大的范围，测试ok
; 2025-11-18	重新整理、测试ok
; 2005-09-29 	v1.0 

(fl_layer_check FLLT_AXIS "134")
(fl_layer_check FLLT_AXNM "7")

(defun get_id_ylist (list_ / id_list id_list_len n)
	(setq 
		id_list (list "A" "B" "C" "D" "E" "F" "G" "H" "J" "K" "L" "M" "N" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z")
		list_len (length list_)
		id_list_len (length id_list)
		n (/ (float list_len) (float id_list_len))
	)
	(if (> n 1)
		(progn
			(setq idx 0 ex_list (list))
			(repeat (fix n)
				(setq
					prefix (nth idx id_list)
					ex_list (append ex_list 
						(mapcar 
							'(lambda (itm_) (strcat prefix itm_))
							id_list
						)
					)
					idx (1+ idx)
				)
			)
			(mapcar 
				'(lambda (_ ex_) ex_) 
				list_ 
				(append id_list ex_list)
			)
		)
		id_list
	)
)

(defun c:aidd (/ ax_sca ax_rad id_lay cu_lay id_y_list ax_line_ss ax_x_list ax_y_list 
			   ax_xpt1_list ax_xpt2_list ax_ypt1_list ax_ypt2_list ss_idx line_ent 
			   line_pt1 line_pt2 ptxy x temp os_mod
			  ) 
	(fl_undo_begin)
	; 用户图层设置
	(setq id_lay FLLT_AXNM
		  ax_lay FLLT_AXIS
	)
	(setq ax_sca (getvar "dimscale")
		  os_mod (getvar "osmode")
		  ax_rad (* ax_sca 4.0)
		  cu_lay (getvar "clayer")
		  dy_mod (if (fl_check_ver 15) (getvar "dynmode") nil)
	)
	(setvar "osmode" 0)
	(setvar "cmdecho" 0)
	(setvar "clayer" id_lay)
	(if dy_mod (setvar "dynmode" 0))

	(if (setq ax_line_ss (ssget (list '(0 . "LINE") (cons 8 ax_lay)))) 
		(progn 
			(setq ax_x_list    (list)
				  ax_y_list    (list)
				  ax_xpt1_list (list)
				  ax_xpt2_list (list)
				  ax_ypt1_list (list)
				  ax_ypt2_list (list)
				  ss_idx       0
			)
			(repeat (sslength ax_line_ss)
				(setq line_ent (entget (ssname ax_line_ss ss_idx))
					  line_pt1 (trans (cdr (assoc 10 line_ent)) 0 1)
					  line_pt2 (trans (cdr (assoc 11 line_ent)) 0 1)
				)
				(cond
					; 纵向
					((equal (setq ptxy (car line_pt1)) (car line_pt2) 0.00001)
					 	(if (member ptxy ax_x_list) 
							nil
							(setq 	ax_xpt1_list (append ax_xpt1_list (list line_pt1))
									ax_xpt2_list (append ax_xpt2_list (list line_pt2))
									ax_x_list   (append ax_x_list (list ptxy))
							)
					 	)
					)
					; 横向
					((equal (setq ptxy (cadr line_pt1)) (cadr line_pt2) 0.00001)
						(if (member ptxy ax_y_list) 
							nil
							(setq 	ax_ypt1_list (append ax_ypt1_list (list line_pt1))
									ax_ypt2_list (append ax_ypt2_list (list line_pt2))
									ax_y_list    (append ax_y_list (list ptxy))
							)
						)
					)
					(t)
				)
				(setq ss_idx (1+ ss_idx))
			)
			(setq id_y_list (if ax_y_list (get_id_ylist ax_y_list)))
			(setq ax_line_ss   nil
				  ax_xpt1_list (vl-sort ax_xpt1_list '(lambda (pt1_ pt2_) (< (car pt1_) (car pt2_))))
				  ax_xpt2_list (vl-sort ax_xpt2_list '(lambda (pt1_ pt2_) (< (car pt1_) (car pt2_))))
				  ax_ypt1_list (vl-sort ax_ypt1_list '(lambda (pt1_ pt2_) (< (cadr pt1_) (cadr pt2_))))
				  ax_ypt2_list (vl-sort ax_ypt2_list '(lambda (pt1_ pt2_) (< (cadr pt1_) (cadr pt2_))))
				  id_x_list    (mapcar 
									'(lambda (x_) (rtos (1+ (vl-position x_ ax_xpt1_list)) 2 0))
									ax_xpt1_list
							   )
			)
			(mapcar 
				'(lambda (pt1_ pt2_ ins_at_ / ins_bk) 
						(setq ins_bk (if (> (strlen ins_at_) 1) "axi" "axi0"))
						(if (> (cadr pt1_) (cadr pt2_)) 
							(setq temp pt1_
								pt1_  pt2_
								pt2_  temp
								temp nil
							)
						)
						(setq pt1_ (subst (- (cadr pt1_) ax_rad) (cadr pt1_) pt1_))
						(setq pt2_ (subst (+ (cadr pt2_) ax_rad) (cadr pt2_) pt2_))
						(cmd "insert" ins_bk pt1_ ax_sca "" 0 ins_at_)
						(cmd "insert" ins_bk pt2_ ax_sca "" 0 ins_at_)
					)
				ax_xpt1_list
				ax_xpt2_list
				id_x_list
			)
			(mapcar 
				'(lambda (pt1_ pt2_ ins_at_ / ins_bk) 
					(setq ins_bk (if (> (strlen ins_at_) 1) "axi" "axi0"))
					(if (> (car pt1_) (car pt2_)) 
						(setq temp pt1_
							pt1_  pt2_
							pt2_  temp
							temp nil
						)
					)
					(setq pt1_ (subst (- (car pt1_) ax_rad) (car pt1_) pt1_))
					(setq pt2_ (subst (+ (car pt2_) ax_rad) (car pt2_) pt2_))
					(cmd "insert" ins_bk pt1_ ax_sca "" 0 ins_at_)
					(cmd "insert" ins_bk pt2_ ax_sca "" 0 ins_at_)
				)
				ax_ypt1_list
				ax_ypt2_list
				id_y_list
			)
		)
	)
	(fl_undo_end)
	(setvar "osmode" os_mod)
	(setvar "cmdecho" 1)
	(setvar "clayer" cu_lay)
	(if dy_mod (setvar "dynmode" dy_mod))
	(princ)
)
(princ "FishLISP C:AIDD ")
(princ)