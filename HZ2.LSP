;2018/5/8

(defun c:2shx ()
	(princ "\n")
	(set_style "txt1.shx" "hztxt.shx")
	(princ)
)

(defun set_style (font_new_ big_new_ / old_amt new_amt xref_amt next_style style_name font_name big_name font_found big_found ft_set)
	(setvar "cmdecho" 0)
	(setvar "regenmode" 0)
	(setq
	 	flag t
		old_amt 0
		new_amt 0
		xref_amt 0
	)
	(while (setq next_style	(tblnext "style" flag))
		(setq
			style_name	(cdr (assoc 2 next_style))
			fmp_list	(list)
		)
		(if (= 16 (logand 16 (cdr (assoc 70 next_style))))
			(setq xref_amt (1+ xref_amt))
			;not a xref style
			(setq
				font_name 	(cdr (assoc 3 next_style))
				big_name 	(cdr (assoc 4 next_style))
				font_found 	(or 
								(findfile font_name)
								(findfile (strcat font_name ".shx"))
								(findfile (strcat font_name ".ttf"))
							)
				big_found 	(or
								(= "" big_name) 
								(findfile big_name) 
								(findfile (strcat big_name ".shx")) 
								(findfile (strcat big_name ".ttf"))
							)
				font_name 	(if font_found font_name font_new_)
				big_name 	(if big_found big_name big_new_)
				ft_set 		(strcat font_name "," big_name)
				ft_set 		(if (= ft_set (strcat font_new_ ","))
								(strcat font_new_ "," big_new_) 
								ft_set
							)
			)
		)
		(if (and font_found big_found)
			(setq old_amt (1+ old_amt))
			(progn
				(setq
					new_amt (1+ new_amt)
					fmp_list (append fmp_list (list font_name))
					fmp_list (append fmp_list (list big_name))
				)
				(cmd "_style" style_name ft_set "0" "" "0")
				(setq style_name nil ft_set nil)
			)
		)
		(while (= "STYLE" (getvar "cmdnames"))
			(cmd "_n")
		)
		(setq flag nil)
	)
	(setvar "regenmode" 1)
	(cmd "_regen")
	(setvar "cmdecho" 1)
	(princ "\n")
	(setq old_s (if (< old_amt 2) "" "s"))
	(princ (strcat (rtos old_amt 2 0) " style" old_s " found. \t"))
	(princ (strcat (rtos new_amt 2 0) " replaced."))
	(if (zerop xref_amt) 
		nil 
		(princ (strcat "\n" (rtos xref_amt 2 0) " xref-style(s) ignored"))
	)
)

(princ "loaded. Start as C:2SHX")
(princ)
