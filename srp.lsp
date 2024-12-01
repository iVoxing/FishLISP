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
				) ;while
				(close txt_f)
			);progn
			(princ "\nFile not found!")
		);if
		(princ "\nText objects replaced! ")
		);progn if t_file
	);if
	(princ)
);defun c:srp



;替换
(defun srp_rp (txt1 txt2 / txt_ss idx txt_en)
	(setq txt_ss (ssget "x" (list (cons 0 "text") (cons 1 txt1))))
	(if txt_ss
		(progn
			(setq idx 0)
			(repeat (sslength txt_ss)
				(setq txt_en (ssname txt_ss idx))
				(entmod (subst (cons 1 txt2) (cons 1 txt1) (entget txt_en)))
				(setq idx (1+ idx))
			);repeat
			(princ (strcat "\n\t" txt1 " -> " txt2 " OK"))
		);progn
	);if txt_ss
);defun srp_rp

(princ "loaded. Start as C:SRP ")
(princ)
