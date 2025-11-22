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
