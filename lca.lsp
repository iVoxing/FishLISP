; FishLISP C:LCA
; 校准LINE或PLINE
; 2025-11-29	rewriting
;				考虑UCS，更加函数化
; 2005-09-16	v1.0

(defun lca_func (ori_pt_ ss_ / mod_idx ent_idx ent_obn ent_obl)
	(setq 
		mod_idx 0
		ent_idx 0
	)
	(repeat (sslength ss_)
		(setq 
			ent_obn (ssname ss_ ent_idx)
			ent_obl (entget ent_obn)
		)
		(entmod
			(mapcar 
				'(lambda (pl_itm_)
					(if (member (car pl_itm_) '(10 11))
						(if (member 
								(mapcar 
									'(lambda (ptx_ pt0_) 
										(rem (- ptx_ pt0_) LCA:SNAP)
									)
									(cdr pl_itm_)
									ori_pt_
								)
								'((0.0 0.0 0.0) (0.0 0.0))
							)
							;顶点无需修改
							pl_itm_
							;替换顶点
							(progn 
								(setq mod_idx (1+ mod_idx))
								(cons (car pl_itm_)
									(mapcar 
										'(lambda (ptx_ pt0_)
											(setq ptx_ (+ ptx_ (* LCA:SNAP 0.5 (if (< ptx_ pt0_) -1 1))))
											(- ptx_ (rem (- ptx_ pt0_) LCA:SNAP))
										)
										(cdr pl_itm_)
										ori_pt_
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
	mod_idx
)

(defun c:lca (/ ori_pt mysnap ent_ss mod_amt)
	(princ "\nFishLISP C:LCA v1.0 Line、Pline校准。")
	(setvar "cmdecho" 0)
	(fl_undo_begin)

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

	(prompt "\n选择图元：")
	(setq ent_ss (ssget '((0 . "LINE,LWPOLYLINE"))))
	(if ent_ss 
		(setq mod_amt (lca_func ori_pt ent_ss))
	)

	(fl_undo_end)
	(setvar "cmdecho" 1)
	(princ (strcat "\n" (itoa mod_amt) "个顶点已经校准。"))
	(princ)
)
(princ "FishLISP C:LCA")
(princ)