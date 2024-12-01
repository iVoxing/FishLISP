; FISHLISP
; C:AIDD
; 根据轴线，自动添加轴线号。
; History:
; 	2005-09-29 v1.0 
;
(setq olderr *error*)
(defun *error* (s)
	(princ "\n缺少文件 Function.lsp，程序不能运行！")
	(setq *error* olderr olderr nil)
	(princ)
)
(if fl_undo_begin
	nil
	(if (findfile "function.lsp")
		(load "function.lsp")
		(exit)
	)
)
(setq *error* olderr olderr nil)

(defun c:aidd (/ ax_sca ax_rad id_lay cu_lay id_y_list ax_line_ss ax_x_list ax_y_list ax_xpt1_list ax_xpt2_list ax_ypt1_list ax_ypt2_list ss_idx line_ent line_pt1 line_pt2 ptxy pt1 pt2 x ins_at ins_bk temp os_mod)
	(fishlisp "aidd" "1.0")
	(fl_undo_begin)
	; 用户图层设置
	(setq 
		id_lay "Paxinum"
		ax_lay "Paxis"
	)

	(setq 
		ax_sca (getvar "dimscale")
		os_mod (getvar "osmode")
		ax_rad (* ax_sca 4.0)
		cu_lay (getvar "clayer")
		dy_mod (if (fl_check_ver 15) (getvar "dynmode") nil)
	)
	(setvar "osmode"	0)
	(setvar "cmdecho" 	0)
	(setvar "clayer" 	id_lay)
	(if dy_mod (setvar "dynmode" 0))
	(setq id_y_list (list "A" "B" "C" "D" "E" "F" "G" "H" "J" "K" "L" "M" "N" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"))
	(if (setq ax_line_ss (ssget (list '(0 . "LINE") (cons 8 ax_lay))))
		(progn
			(fl_bench_begin)
			(setq 
				ax_x_list (list)
				ax_y_list (list)
				ax_xpt1_list (list)
				ax_xpt2_list (list)
				ax_ypt1_list (list)
				ax_ypt2_list (list)
				ss_idx 0
			);setq
			(repeat (sslength ax_line_ss)
				(setq 
					line_ent (entget (ssname ax_line_ss ss_idx))
					line_pt1 (trans (cdr (assoc 10 line_ent)) 0 1)
					line_pt2 (trans (cdr (assoc 11 line_ent)) 0 1)
				)
				(cond
					((equal (setq ptxy (car line_pt1)) (car line_pt2) 0.00001)
						(if (member ptxy ax_x_list)
							nil
							(setq 
								ax_xpt1_list (append ax_xpt1_list (list line_pt1))
								ax_xpt2_list (append ax_xpt2_list (list line_pt2))
								ax_x_list (append ax_x_list (list ptxy))
							)
						)
					)
					((equal (setq ptxy (cadr line_pt1)) (cadr line_pt2) 0.00001)
						(if (member ptxy ax_y_list)
							nil
							(setq 
								ax_ypt1_list (append ax_ypt1_list (list line_pt1))
								ax_ypt2_list (append ax_ypt2_list (list line_pt2))
								ax_y_list (append ax_y_list (list ptxy))
							)
						)
					)
					( t 
					)
				);cond
				(setq ss_idx (1+ ss_idx))
			);repeat
			(setq 
				ax_line_ss nil
				ax_xpt1_list (vl-sort ax_xpt1_list '(lambda (pt1 pt2) (< (car pt1) (car pt2))))
				ax_xpt2_list (vl-sort ax_xpt2_list '(lambda (pt1 pt2) (< (car pt1) (car pt2))))
				ax_ypt1_list (vl-sort ax_ypt1_list '(lambda (pt1 pt2) (< (cadr pt1) (cadr pt2))))
				ax_ypt2_list (vl-sort ax_ypt2_list '(lambda (pt1 pt2) (< (cadr pt1) (cadr pt2))))
				id_x_list (mapcar '(lambda (x) (rtos (1+ (vl-position x ax_xpt1_list)) 2 0)) ax_xpt1_list)
			)
			(mapcar
				'(lambda (pt1 pt2 ins_at)
					(setq ins_bk (if (> (strlen ins_at) 1) "axi" "axi0"))
					(if (> (cadr pt1) (cadr pt2))
						(setq temp pt1 pt1 pt2 pt2 temp temp nil)
					)
					(setq pt1 (subst (- (cadr pt1) ax_rad) (cadr pt1) pt1))
					(setq pt2 (subst (+ (cadr pt2) ax_rad) (cadr pt2) pt2))
					(cmd "insert" ins_bk pt1 ax_sca "" 0 ins_at)
					(cmd "insert" ins_bk pt2 ax_sca "" 0 ins_at)
				);lambda
				ax_xpt1_list
				ax_xpt2_list
				id_x_list
			);mapcar
			(mapcar
				'(lambda (pt1 pt2 ins_at)
					(setq ins_bk (if (> (strlen ins_at) 1) "axi" "axi0"))
					(if (> (car pt1) (car pt2))
						(setq temp pt1 pt1 pt2 pt2 temp temp nil)
					)
					(setq pt1 (subst (- (car pt1) ax_rad) (car pt1) pt1))
					(setq pt2 (subst (+ (car pt2) ax_rad) (car pt2) pt2))
					(cmd "insert" ins_bk pt1 ax_sca "" 0 ins_at)
					(cmd "insert" ins_bk pt2 ax_sca "" 0 ins_at)
				);lambda
				ax_ypt1_list
				ax_ypt2_list
				id_y_list
			);mapcar
		);progn ax_line_ss
	);if
	(setvar "osmode" os_mod)
	(setvar "cmdecho" 1)
	(setvar "clayer" cu_lay)
	(if dy_mod (setvar "dynmode" dy_mod))
	(fl_bench_end)
	(fl_undo_end)
	(princ)
)
(princ "FishLISP C:AIDD ")
(princ)