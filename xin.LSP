; 2004-03-15	1.1	修改直接输入图块名称无效的错误。

(defun c:xin (/ blk_name pth_name blk_exis ins_name blk_enti blk_type)
	(setq blk_name (getstring "\n图块名称[选择图块]："))
	(if (= "" blk_name)
		(setq blk_enti (entsel "\n选择图块："))
	)
	(if blk_enti
		(setq 
			blk_enti (entget (car blk_enti))
			blk_type (cdr (assoc 0 blk_enti))
		)
	)
	(setq blk_name 
		(if (= blk_type "INSERT")
			(cdr (assoc 2 blk_enti))
			blk_name
		)
	)
	(if blk_name
		(progn
			(setq pth_name (findfile (strcat blk_name ".dwg")))
			(setq blk_exis (tblsearch "block" blk_name))
			(setq ins_name nil)
		)
	)
	(cond
		((and blk_exis pth_name)
			(setq ins_name (strcat blk_name "=" pth_name))
		)
		( blk_exis
			(alert "没有找到同名的外部图块")
		)
		( pth_name
			(alert "没有该内部图块")
		)
		(t
			(alert "没有该图块")
		)
	)
	(if ins_name
		(progn
			(setvar "cmdecho" 0)
			(cmd "_insert" ins_name "0,0" "" "" "")
			(entdel (entlast))
			(setvar "cmdecho" 1)
			(alert (strcat "图块" (strcase blk_name) "已经更新！"))
		)
	)
	(princ)
)