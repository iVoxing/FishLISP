(defun c:mpub (\ mpub_s mpub_l mpub_pt1 mpub_pt2 n mpub_nam)
	(if (tblsearch "layer" "FL_MPUB")
		(if (setq mpub_s (ssget "x" '((0 . "LINE")(8 . "FL_MPUB"))))
			(progn
				(setq mpub_a (sslength mpub_s) n 0)
				(repeat mpub_a
					(setq	
						mpub_l		(entget (ssname mpub_s n))
						mpub_pt1	(cdr (assoc 10 mpub_l))
						mpub_pt2	(cdr (assoc 11 mpub_l))
						mpub_nam	(strcat (getvar "dwgname") "_" (rtos n 2 0))
					)
					(command "-plot"
						"y"
						""
						"DWG To PDF"
						""
						"m"
						"l"
						"n"
						"w"
						mpub_pt1 mpub_pt2
						"F"
						"C"
						""
						"ACAD.CTB"
						"N"
						""
						mpub_nam
						"N"
						"Y"
					)
					(setq n (1+ n))
				)
			)
		)
	)
	(princ)
)
