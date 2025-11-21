;文本替换工具

;打开文件

(defun c:srp (/ t_file txt_f txt1 txt2)
	(setq t_file (getstring "\nTEXT file name: "))
	(if (= t_file "")
		(princ)
		(progn
			(setq t_file (strcat t_file ".txt"))
			(if (setq t_file (findfile t_file))
				(progn
					(setq txt_f (open t_file "r"))
					(while (setq txt1 (read-line txt_f))
						(setq txt2 (read-line txt_f))
						(setq txt2 (if txt2 txt2 txt1))
						(srp_rp txt1 txt2)
					)
					(close txt_f)
				)
				(princ "\nFile not found!")
			)
			(princ "\nText objects replaced! ")
		)
	)
	(princ)
)



;替换
(defun srp_rp (txt1_ txt2_ / txt_ss idx txt_en)
	(setq txt_ss (ssget "x" (list (cons 0 "text") (cons 1 txt1_))))
	(if txt_ss
		(progn
			(setq idx 0)
			(repeat (sslength txt_ss)
				(setq txt_en (ssname txt_ss idx))
				(entmod (subst (cons 1 txt2_) (cons 1 txt1_) (entget txt_en)))
				(setq idx (1+ idx))
			)
			(princ (strcat "\n\t" txt1_ " -> " txt2_ " OK"))
		)
	)
)

(princ "loaded. Start as C:SRP ")
(princ)
