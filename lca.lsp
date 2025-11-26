;FishLISP C:LCA
;校准LINE或PLINE
;2005-09-16v1.0

(defun c:lca (/ ori_pt mysnap ent_obn ent_obl ent_ss ent_idx mod_idx)
	(princ "\nFishLISP C:LCA v1.0 Line、Pline校准。")
	(setvar "cmdecho" 0)
	(cmd "undo" "begin")

	;设置原点，缺省0,0
	(setq LCA:ORI_PT (if LCA:ORI_PT LCA:ORI_PT '(0.0 0.0 0.0)))
	(setq msg (strcat 
		"\n设置基准点：<"
		(rtos (car LCA:ORI_PT))
		","
		(rtos (cadr LCA:ORI_PT))
		","
		(rtos (caddr LCA:ORI_PT))
		"> "
	))
	(setq ori_pt (getpoint msg))
	(setq ori_pt (if ori_pt ori_pt LCA:ORI_PT))
	(setq LCA:ORI_PT ori_pt)

	;设置校准精度，即模数，缺省10
	(setq LCA:SNAP (if LCA:SNAP LCA:SNAP 10))
	(setq mysnap (getint (strcat "\n设置校准精度：<" (rtos LCA:SNAP) "> ")))
	(setq mysnap (if mysnap mysnap LCA:SNAP))
	(setq LCA:SNAP mysnap)

	;收集图元
	(prompt "\n选择图元：")
	(setq ent_ss (ssget '((0 . "LINE,LWPOLYLINE"))))
	(if ent_ss
		(progn
			(setq 
				mod_idx 0
				ent_idx 0
			)
			(repeat (sslength ent_ss)
				(setq 
					ent_obn (ssname ent_ss ent_idx)
					ent_obl (entget ent_obn)
				)
				;替换图元
				(entmod
					(mapcar 
						'(lambda (pl_itm_)
							(if (member (car pl_itm_) (list 10 11))
								(if 
									(member 
										(mapcar '(lambda (ptx_ pt0_) (rem (- ptx_ pt0_) LCA:SNAP)) (cdr pl_itm_) ori_pt)
										(list '(0.0 0.0 0.0) '(0.0 0.0))
									)
									;顶点无需修改
									pl_itm_
									;替换顶点
									(progn 
										(setq mod_idx (1+ mod_idx))
										(cons (car pl_itm) 
											(mapcar 
												'(lambda (ptx_ pt0_)
													(setq ptx_ (+ ptx_ (* LCA:SNAP 0.5 (if (< ptx_ pt0_) -1 1))))
													(- ptx_ (rem (- ptx_ pt0_) LCA:SNAP))
												)
												(cdr pl_itm_) ori_pt
											)
										)
									)
								)
								pl_itm_
							)
						)
						ent_obl
					)
				)
				(entupd ent_obn)
				(setq ent_idx (1+ ent_idx))
			)
		)
	)
	(cmd "undo" "end")
	(setvar "cmdecho" 1)
	(princ (strcat "\n" (rtos mod_idx 2 0) "个顶点已经校准。"))
	(princ)
)
(princ "FishLISP C:LCA")
(princ)