(set 'cmd (if (type command-s) command-s command))

(defun fl_check_ver (v_)
	(if (>= (read (substr (getvar "acadver") 1 2)) v_) t nil)
)

;设置UNDO起点
(defun fl_undo_begin (/ cmdstat) 
	(setq cmdstat (getvar "cmdecho"))
	(setvar "cmdecho" 0)
	(if (fl_check_ver 16) 
		(cmd "undo" "group")
		(cmd "undo" "begin")
	)
	(setvar "cmdecho" cmdstat)
)

;设置UNDO终点
;在命令结束和ERROR处理时，均可使用
(defun fl_undo_end (/ cmdstat) 
	(setq cmdstat (getvar "cmdecho"))
	(setvar "cmdecho" 0)
	(cmd "undo" "end")
	(setvar "cmdecho" cmdstat)
)

;获取Pline顶点列表
(defun fl_get_plvtx (pl_en_ / vtx_list itm) 
	(setq vtx_list (list))
	(foreach itm (entget pl_en_)
		(if (= (car itm) 10) 
			(setq vtx_list (append vtx_list (list (cdr itm))))
		)
	)
	vtx_list
)

;将选择集转换为实体列表
(defun fl_ss_2_list (ss_ / objlist idx) 
	(setq 
		objlist (list)
		idx 0
	)
	(repeat (sslength ss_) 
		(setq 
			objlist (append objlist (list (ssname ss_ idx)))
			idx (1+ idx)
		)
	)
	objlist
)

; 获取Line选择集的顶点列表
; 参数：
; ssLine实体选择集
; code10列出起点，11列出终点，其他值或空则同时列出两点
(defun fl_line2ptlist (ss_ code_) 
	(cond 
		((member code_ (list 10 11))
		 	(mapcar 
		 		'(lambda (x_) (cdr (assoc code_ (entget x_))))
				(fl_ss_2_list ss_)
		 	)
		)
		(t
			(mapcar 
				'(lambda (x_) (list (cdr (assoc 10 (entget x_))) (cdr (assoc 11 (entget x_)))))
				(fl_ss_2_list ss_)
			)
		)
	)
)

(defun fl_bench_begin () 
	(setq fl_bench_tm1 (getvar "date"))
)

(defun fl_bench_end () 
	(if fl_bench_tm1 
		(progn 
			(princ "\nTime: ")
			(setq fl_bench_tm2 (getvar "date"))
			(princ (* 86400.0 (- fl_bench_tm2 fl_bench_tm1)))
			(princ "s.")
			(setq fl_bench_tm2 nil fl_bench_tm1 nil)
		)
	)
)

(defun fl_layer_check (name_ color_) 
	(setvar "cmdecho" 0)
	(if (tblsearch "Layer" name_) 
		nil
		(cmd "layer" "new" name_ "color" color_ name_ "")
	)
	(setvar "cmdecho" 1)
)

(defun fl_var_set (var_name_ var_value_)
	(append var_list (list (list var_name_ (getvar var_name_))))
	(setvar var_name_ var_value_)
)

(defun fl_var_res ()
	(mapcar
		'(lambda (var_) (setvar (car var_) (cadr var_)))
		var_list
	)
	(setq var_list (list))
)

(defun fl_make_modi_layer (/ lname unplt)
	(setvar "cmdecho" 0)
	(setq lname (strcat "modi" (substr (rtos (getvar "cdate")) 3 6)))
	(setq unplt (not (tblsearch "layer" lname)))
	(cmd "_.layer" "m" lname "")
	(if unplt
		(cmd "_.layer" "c" "r" "" "plot" "n" "" "")
	)
	(setvar "cmdecho" 1)
)

; 将字符串列表用分隔符连接成一个字符串
; 可配合 get_laylist_by_state，或别的函数
; 2025-11-21
(defun fl_strlist_2_string (str_list_ sep_)
	(if str_list_ (apply 'strcat (mapcar '(lambda (x_) (strcat x_ sep_)) str_list_)) "")
)

; 感觉 get_laystr_by_state 更直接、实用
; 2025-11-21	初版，保留一段时间，如无调用可删除
(defun get_laylist_by_state (state_ / state_list state_d lay_list lay_tbl)
	(setq state_list '(("FREEZE" 1) ("LOCK" 4)))
	(setq state_d (cadr (assoc (strcase state_) state_list)))
	(setq 
		lay_list (list)
		lay_tbl (tblnext "layer" t)
	)
	(while lay_tbl
		(if (= (logand (cdr (assoc 70 lay_tbl)) state_d) state_d)
			(setq lay_list (append lay_list (list (cdr (assoc 2 lay_tbl)))))
		)
		(setq lay_tbl (tblnext "layer"))
	)
	lay_list
)

; 2025-11-21
(defun get_laystr_by_state (state_ / state_list state_d lay_str lay_tbl)
	(setq state_list '(("FREEZE" 1) ("LOCK" 4)))
	(setq state_d (cadr (assoc (strcase state_) state_list)))
	(setq 
		lay_str ""
		lay_tbl (tblnext "layer" t)
	)
	(while lay_tbl
		(if (= (logand (cdr (assoc 70 lay_tbl)) state_d) state_d)
			(setq lay_str (strcat lay_str (cdr (assoc 2 lay_tbl)) ","))
		)
		(setq lay_tbl (tblnext "layer"))
	)
	(if (= lay_str "") nil lay_str)
)

(princ)
