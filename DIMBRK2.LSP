(defun dim_brk (lin_en_ / os oo cl lin_ent lin_pt1 lin_pt2 dim_ss dim_amt dim_idx dim_ent dim_pt0 dim_lay dim_pt1 dim_pt2 dim_hv dim_int)
	(setq 
		lin_ent (entget lin_en_)
		lin_pt1 (cdr (assoc 10 lin_ent))
		lin_pt2 (cdr (assoc 11 lin_ent))
		dim_ss (ssget "f" (list lin_pt1 lin_pt2) '((0 . "dimension")))
		os (getvar "osmode")
		oo (getvar "orthomode")
		cl (getvar "clayer")
	)
	(if dim_ss
		(progn 
			(setq 
			 	dim_amt (sslength dim_ss)
				dim_idx 0
			)
			(repeat dim_amt
				(setq 
					dim_ent (entget (ssname dim_ss dim_idx))
					dim_pt0 (cdr (assoc 10 dim_ent)) 
					dim_lay (cdr (assoc 8 dim_ent)) 
					dim_pt1 (cdr (assoc 13 dim_ent))
					dim_pt2 (cdr (assoc 14 dim_ent))
					dim_hv (cdr (assoc 50 dim_ent))
					dim_int (inters dim_pt1 dim_pt2 lin_pt1 lin_pt2)
				)
				(setvar "osmode" 0)
				(setvar "orthomode" 0)
				(setvar "clayer" dim_lay)
				(cmd "_erase" (cdr (assoc -1 dim_ent)) "")
				(cmd "_dim1" (if (= dim_hv 0.0) "hor" "ver") dim_pt1 dim_int dim_pt0 "")
				(cmd "_dim1" (if (= dim_hv 0.0) "hor" "ver") dim_int dim_pt2 dim_pt0 "")
				(setq dim_idx (1+ dim_idx))
			)
		)
	)
	(setvar "osmode" os)
	(setvar "orthomode" oo)
	(setvar "clayer" cl)
)

(defun c:dimbrk (/ lin_ss lin_amt lin_idx lin_en)
	(setvar "cmdecho" 0)
	(fl_undo_begin)
	(prompt "\nSelect line to break dimension: ")
	(setq lin_ss (ssget '((0 . "line"))))
	(if lin_ss
		(progn
			(setq 
			 	lin_amt (sslength lin_ss)
				lin_idx 0
		 	)
		 	(repeat lin_amt
			 	(setq lin_en (ssname lin_ss lin_idx))
			 	(dim_brk lin_en)
			 	(setq lin_idx (1+ lin_idx))
		 	)
		)
	) 
	(fl_undo_end)
	(redraw)
	(setvar "cmdecho" 1)
	(princ)
)