; FISHLISP
; C:
; Description
; History:
; yy-mm-dd v#.# ???
(defun c:oww (/ dist loop en0 ent enttyp en0pt0 en0pt1 en0pt2 en0ang ss dirpt0 x0 y0 x2 y2 x3 y3 dirang newpt1 newpt2 idx en1 ent1 ent1ty)
	(if ofwl_dist nil (setq ofwl_dist 0.0))
	(princ "\nOffset distance: <")
	(princ ofwl_dist)
	(setq dist (getdist "> "))
	(if dist nil (setq dist ofwl_dist))
	(setq ofwl_dist dist)

	(setq loop t)
	(while loop
		(setq en0 (entsel "\nSelect Line to offset: "))
		(if en0
			(setq 
				ent (entget (car en0))
				enttyp (cdr (assoc 0 ent))
				en0pt0 (cadr en0)
			)
		)
		(cond
			((not en0) (setq loop nil))
			((/= enttyp "LINE") (princ "\n1 was not line. "))
			(t
				(fl_undo_begin)
				(setq en0pt1 (cdr (assoc 10 ent))
					en0pt2 (cdr (assoc 11 ent))
					en0ang (angle en0pt1 en0pt2)
				)
				(setq ss (ssget "f" (list en0pt1 en0pt2)))
				;ss (ssget "p" '((0 . "LINE")))
				(setq dirpt0 (getpoint "Specify point on side to offset: ")
					x0 (car en0pt0)
					y0 (cadr en0pt0)
					x1 (car en0pt1)
					y1 (cadr en0pt1)
					x2 (car en0pt2)
					y2 (cadr en0pt2)
					x3 (car dirpt0)
					y3 (cadr dirpt0)
					dirang (if (< (* (- x2 x0) (- y3 y0)) (* (- x3 x0) (- y2 y0)));用矢量叉积判断应取角度
								(- en0ang (/ pi 2))
								(+ en0ang (/ pi 2))
							);if
					newpt1 (polar en0pt1 dirang dist)
					newpt2 (polar en0pt2 dirang dist)
					idx 0
				)
				(repeat (sslength ss)
					(setq en1 (ssname ss idx)
							ent1 (entget en1)
							ent1ty (cdr (assoc 0 ent1))
					)
					(cond
						((= ent1ty "LINE")
							(setq 
								ent1 (subst (cons 10 newpt1) (cons 10 en0pt1) ent1)
								ent1 (subst (cons 11 newpt1) (cons 11 en0pt1) ent1)
								ent1 (subst (cons 10 newpt2) (cons 10 en0pt2) ent1)
								ent1 (subst (cons 11 newpt2) (cons 11 en0pt2) ent1)
								idx (1+ idx)
							)
						);line
						((= ent1ty "LWPOLYLINE")
							(setq 
								ent1 (mapcar '(lambda (pl_itm)
									(cond
										((/= (car pl_itm) 10)
											pl_itm
										)
										((= pl_itm (list 10 (car en0pt1) (cadr en0pt1)))
											(cons 10 (list (car newpt1) (cadr newpt1)))
										)
										((= pl_itm (list 10 (car en0pt2) (cadr en0pt2)))
											(cons 10 (list (car newpt2) (cadr newpt2)))
										)
										(t
											(princ ">")
											pl_itm
										)
									);cond
								));mapcar
								idx (1+ idx)
							)
						);pline
						(t)
					);cond
					(entmod ent1)
					(entupd en1)
				);repeat
				(fl_undo_end)
			);cond t
		);cond
	);while
	(princ)
)
(princ "FishLISP C:OWW ")
(princ)