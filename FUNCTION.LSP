(set 'cmd (if (type command-s) command-s command))

; 交换变量值
(defun fl_exchange (var1 var2 / temp) 
	(setq 
		temp var1
		var1 var2
		var2 temp
	)
)

(defun fl_check_ver (v)
	(if (>= (read (substr (getvar "acadver") 1 2)) v) t nil)
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

;获取点的X值
(defun fl_get_x (pt) (car pt))

;获取点的Y值
(defun fl_get_y (pt) (cadr pt))

;获取点的Z值，如果没有Z值则填零
(defun fl_get_z (pt / zvalue) 
	(setq 
		zvalue (caddr pt)
		zvalue (if zvalue zvalue 0.0)
	)
)

;获取Pline顶点列表
(defun fl_get_plvtx (pl_ent / vtx_list itm) 
	(setq vtx_list (list))
	(foreach itm pl_ent 
		(if (= (car itm) 10) 
			(setq vtx_list (append vtx_list (list (cdr itm))))
		)
	)
	vtx_list
)

;抽取一组LIST的每第N个元素形成一个新表
;可以用于抽取一组点的X列表，Y列表等
;参数：
;list_list	由表组成的表，比如((1 10 100) (2 20 200) (3 30 300))
;n		抽取位置，0为第一位
(defun fl_get_nth (list_list n) 
	(mapcar '(lambda (x) (nth n x)) list_list)
)

;将选择集转换为实体列表
(defun fl_ss2objlist (ss / objlist idx) 
	(setq 
		objlist (list)
		idx 0
	)
	(repeat (sslength ss) 
		(setq 
			objlist (append objlist (list (ssname ss idx)))
			idx (1+ idx)
		)
	) ;repeat
	objlist
)

;获取Line选择集的顶点列表
;参数：
; ssLine实体选择集
; code10列出起点，11列出终点，其他值或空则同时列出两点
(defun fl_line2ptlist (ss code) 
	(cond 
		((member code (list 10 11))
		 (mapcar 
		 	'(lambda (x) (cdr (assoc code (entget x))))
			(fl_ss2objlist ss)
		 )
		)
		(t
		 (mapcar 
			'(lambda (x) (list (cdr (assoc 10 (entget x))) (cdr (assoc 11 (entget x)))))
			(fl_ss2objlist ss)
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
		) ;progn
	) ;if
)

(defun fl_layer_check (lay_name lay_clr) 
	(if (tblsearch "Layer" lay_name) 
		nil
		(progn 
			(setvar "cmdecho" 0)
			(cmd "layer" "new" lay_name "color" lay_clr lay_name "")
			(setvar "cmdecho" 1)
		)
	)
)

(defun fl_var_set (var_name var_value)
	(append var_list (list (list var_name (getvar var_name))))
	(setvar var_name var_value)
)

(defun fl_var_res ()
	(mapcar
		'(lambda (var) (setvar (car var) (cadr var)))
		var_list
	)
	(setq var_list (list))
)

(defun c:fl_list ()
	(princ (strcat
		"\ncmd\tIf COMMAND-S available use COMMAND-S, otherwise COMMAND."
		"\nfl_get_nth\tMake new list with nth itiem of inner list in outer list."
		"\nfl_get_plvtx\tGet points list in PLINE."
		"\nfl_get_x"
		"\nfl_get_y"
		"\nfl_get_z"
		"\nfl_layer_check\tCheck layer, if not available, make it with certain color."
		"\nfl_make_modi_layer\tMake a layer named as MODIyymmdd."
		"\nfl_line2ptlist\tLINE entities selection to Points list."
		"\nfl_ss2objlist\tEntities selection to list."
		"\nfl_undo_begin"
		"\nfl_undo_end"
		"\nfl_var_res\tRestore system varable to old value saved by fl_var_set."
		"\nfl_var_set\tSet system varable and save old value."
	))
	(princ)
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

(princ)
