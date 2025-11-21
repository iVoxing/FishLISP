(defun c:algdim (/ k ss idx pt0 ent)
	(initget "?")
	(setq k (getkword "\n?/<Select Dimension objects>: "))
	(if (= k "?") (algdimhlp))
	(setq 
		ss (ssget '((0 . "dimension") (-4 . "<or") (70 . 0) (70 . 1) (70 . 32) (70 . 33) (70 . 160) (70 . 161) (-4 . "or>")))
		idx 0
	)
	(if ss
		(if (setq pt0 (getpoint "\nSelect point for new position: "))
			(repeat (sslength ss)
				(setq ent (entget (ssname ss idx)))
				(entmod (subst (cons 10 pt0) (assoc 10 ent) ent))
				(setq idx (1+ idx))
			)
		)
	)
	(princ)
)

(defun algdimhlp ()
	(alert (strcat
		"C:ALGDIM								Version 1.0\n"
		"\n对齐线性标注的标注线。\n"
		"\n使用方法："
		"\n	命令行提示：						操作："
		"\n	1.	?/<Select Dimension objects>:	输入\"？\"显示本帮助对话框，回车继续"
		"\n	2.	Select objects:					选择标注实体（Dimension）"
		"\n										提示：程序有过滤功能，可自动排除非线性标注实体"
		"\n										注意：不要一次选多种类型"
		"\n	3.	Select point for new postion:	选择新的标注位置"
	))
)

(algdimhlp)
(princ "loaded. ")
(princ)