; FISHLISP
; C:AIDD
; 根据轴线，自动添加轴线号。
; History:
; 2005-09-28 v0.9 
;
(setq olderr *error*)
(defun *error* (s)
	(princ "\n缺少文件Function.lsp，程序不能运行！")
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

(defun c:dl ()
	(fl_undo_begin)
	;<<用户图层设置
	(setq id_lay "Paxinum"
				lay "Paxis"
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
			);setq
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
				);cond
				(setq ss_idx (1+ ss_idx))
			);repeat
			(setq 
				line_ss nil
				xpt1_list (vl-sort xpt1_list '(lambda (pt1 pt2) (< (car pt1) (car pt2))))
				xpt2_list (vl-sort xpt2_list '(lambda (pt1 pt2) (< (car pt1) (car pt2))))
				ypt1_list (vl-sort ypt1_list '(lambda (pt1 pt2) (< (cadr pt1) (cadr pt2))))
				ypt2_list (vl-sort ypt2_list '(lambda (pt1 pt2) (< (cadr pt1) (cadr pt2))))
				id_x_list (mapcar '(lambda (x) (rtos (1+ (vl-position x xpt1_list)) 2 0)) xpt1_list)
			)
			(mapcar
				'(lambda (pt1 pt2 ins_at)
					(setq ins_bk (if (> (strlen ins_at) 1) "axi" "axi0"))
					(if (> (cadr pt1) (cadr pt2))
						(setq temp pt1 pt1 pt2 pt2 temp temp nil)
					)
					(setq pt1 (subst (- (cadr pt1) rad) (cadr pt1) pt1))
					(setq pt2 (subst (+ (cadr pt2) rad) (cadr pt2) pt2))
					(cmd "insert" ins_bk pt1 sca "" 0 ins_at)
					(cmd "insert" ins_bk pt2 sca "" 0 ins_at)
				);lambda
				xpt1_list
				xpt2_list
				id_x_list
			);mapcar
			(mapcar
				'(lambda (pt1 pt2 ins_at)
					(setq ins_bk (if (> (strlen ins_at) 1) "axi" "axi0"))
					(if (> (car pt1) (car pt2))
						(setq temp pt1 pt1 pt2 pt2 temp temp nil)
					)
					(setq pt1 (subst (- (car pt1) rad) (car pt1) pt1))
					(setq pt2 (subst (+ (car pt2) rad) (car pt2) pt2))
					(cmd "insert" ins_bk pt1 sca "" 0 ins_at)
					(cmd "insert" ins_bk pt2 sca "" 0 ins_at)
				);lambda
				ypt1_list
				ypt2_list
				id_y_list
			);mapcar
		);progn line_ss
	);if
	(setvar "cmdecho" 1)
	(setvar "clayer" cu_lay)
	(fl_undo_end)
	(princ)
)
(princ "FishLISP C:AIDD ")
(princ)