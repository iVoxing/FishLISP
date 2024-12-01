;FishLISP C:LCA
;校准LINE或PLINE
;2005-09-16v1.0

(defun c:lca (/ ori_pt mysnap ent_obn ent_obl ent_ss ent_idx mod_idx)
	(princ "\nFishLISP C:LCA v1.0 Line、Pline校准。")
	(setvar "cmdecho" 0)
	(cmd "undo" "begin")

	;设置原点，缺省0,0
	(setq lca:ori_pt (if lca:ori_pt lca:ori_pt '(0.0 0.0 0.0)))
	(princ (strcat 
		"\n设置基准点：<"
		(rtos (car lca:ori_pt))
		","
		(rtos (cadr lca:ori_pt))
		","
		(rtos (caddr lca:ori_pt))
		"> "
	));princ
	(setq ori_pt (getpoint))
	(setq ori_pt (if ori_pt ori_pt lca:ori_pt))
	(setq lca:ori_pt ori_pt)

	;设置校准精度，即模数，缺省10
	(setq lca:snap (if lca:snap lca:snap 10))
	(princ "\n设置校准精度：<")
	(princ lca:snap)
	(setq mysnap (getint "> "))
	(setq mysnap (if mysnap mysnap lca:snap))
	(setq lca:snap mysnap)

	;收集图元
	(prompt "\n选择图元：")
	(setq ent_ss (ssget '((0 . "line,lwpolyline"))))
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
						'(lambda (pl_itm)
							(if (member (car pl_itm) (list 10 11))
								(if 
									(member 
										(mapcar '(lambda (ptx pt0) (rem (- ptx pt0) lca:snap)) (cdr pl_itm) ori_pt)
										(list '(0.0 0.0 0.0) '(0.0 0.0))
									)
									;顶点无需修改
									pl_itm
									;替换顶点
									(progn 
										(setq mod_idx (1+ mod_idx))
										(cons (car pl_itm) 
											(mapcar 
												'(lambda (ptx pt0)
													(setq ptx (+ ptx (* lca:snap 0.5 (if (< ptx pt0) -1 1))))
													(- ptx (rem (- ptx pt0) lca:snap))
												)
												(cdr pl_itm) ori_pt
											);mapcar
										);cons
									);progn
								);if 
								pl_itm
							);if
						);lambda
						ent_obl
					);mapcar
				);entmod ent_obl
				(entupd ent_obn)
				(setq ent_idx (1+ ent_idx))
			);repeat ent_ss
		);progn ent_ss 
	);if ent_ss
	(cmd "undo" "end")
	(setvar "cmdecho" 1)
	(princ (strcat "\n" (rtos mod_idx 2 0) "个顶点已经校准。"))
	(princ)
)
(princ "FishLISP C:LCA")
(princ)