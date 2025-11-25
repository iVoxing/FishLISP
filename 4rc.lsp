; Draw 4 rectangles, use to set dimension base range
; 2025-11-24	pure function, test ok
; 2025-11-18	a little optimization
; 2025-11-12 	rewrite with foreach, tested ok

(defun c:4rc (/ dist pt1 pt2 pt3 pt4 minx maxx miny maxy n_dis os)
	(setq 
		pt1 (getpoint "\nUpper: ")
		dist (* (getvar "dimscale") 10)
		dist (if (zerop dist) 1000 dist)
	)
	(if pt1 (setq pt2 (getpoint "\nLower: ")))
	(if pt2 (setq pt3 (getpoint "\nLeft: ")))
	(if pt3 (setq pt4 (getpoint "\nRight: ")))
	(if (not pt4) (exit))
  
	(setq 
		minx (car pt3)
		maxx (car pt4)
		miny (cadr pt2)
		maxy (cadr pt1)
	)
  
	(foreach n (list 0 1 2 3)
		(setq
			n_dis (* n dist)
			pt1 (list (- minx n_dis) (- miny n_dis))
			pt2 (list (+ maxx n_dis) (- miny n_dis))
			pt3 (list (+ maxx n_dis) (+ maxy n_dis))
			pt4 (list (- minx n_dis) (+ maxy n_dis))
        )
		(fl_make_pline (list pt1 pt2 pt3 pt4) 1)
    )

	(princ)
)

(princ "loaded. Start as C:4RC ")
(princ)