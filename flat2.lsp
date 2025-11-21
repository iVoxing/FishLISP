;FishLISP C:FLAT
;对图元的10、11组码Z值填零。
;2005-10-08v2.0
;
;

(defun c:lca (/ ori_pt mysnap ent_obn ent_obl ent_ss ent_idx mod_idx)
	(princ "\nFishLISP C:lca v2.0 对图元的10、11组码Z值填零。")
	(setvar "cmdecho" 0)
	(cmd "undo" "begin")
	(prompt "\n选择图元：")
	(setq ent_ss (ssget))
	(if ent_ss
		(progn
			(setq ent_idx 0)
			(repeat (sslength ent_ss)
				(setq 
					ent_obn (ssname ent_ss ent_idx)
					ent_obl (entget ent_obn)
				)
				;替换图元
				(entmod
					(mapcar 
						'(lambda (pl_itm_ / pt_vul z_vul)
							(setq pt_vul (cdr pl_itm_))
							(cond
								((member (car pl_itm_) (list 10 11))
									(if (and (setq z_vul (caddr pt_vul)) (not (zerop z_vul)))
										pl_itm_
										(subst 0.0 z_vul pl_itm_)
									)
								)
								((member (car pl_itm_) (list 38))
									(subst 0.0 (cadr pt_vul) pl_itm_)
								)
								(t
									pl_itm_
								)
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
	(princ)
)
(princ "FishLISP C:LCA")
(princ)

