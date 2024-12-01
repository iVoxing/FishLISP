;FishLISP C:LCA
;У׼LINE��PLINE
;2005-09-16v1.0

(defun c:lca (/ ori_pt mysnap ent_obn ent_obl ent_ss ent_idx mod_idx)
	(princ "\nFishLISP C:LCA v1.0 Line��PlineУ׼��")
	(setvar "cmdecho" 0)
	(cmd "undo" "begin")

	;����ԭ�㣬ȱʡ0,0
	(setq lca:ori_pt (if lca:ori_pt lca:ori_pt '(0.0 0.0 0.0)))
	(princ (strcat 
		"\n���û�׼�㣺<"
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

	;����У׼���ȣ���ģ����ȱʡ10
	(setq lca:snap (if lca:snap lca:snap 10))
	(princ "\n����У׼���ȣ�<")
	(princ lca:snap)
	(setq mysnap (getint "> "))
	(setq mysnap (if mysnap mysnap lca:snap))
	(setq lca:snap mysnap)

	;�ռ�ͼԪ
	(prompt "\nѡ��ͼԪ��")
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
				;�滻ͼԪ
				(entmod
					(mapcar 
						'(lambda (pl_itm)
							(if (member (car pl_itm) (list 10 11))
								(if 
									(member 
										(mapcar '(lambda (ptx pt0) (rem (- ptx pt0) lca:snap)) (cdr pl_itm) ori_pt)
										(list '(0.0 0.0 0.0) '(0.0 0.0))
									)
									;���������޸�
									pl_itm
									;�滻����
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
	(princ (strcat "\n" (rtos mod_idx 2 0) "�������Ѿ�У׼��"))
	(princ)
)
(princ "FishLISP C:LCA")
(princ)