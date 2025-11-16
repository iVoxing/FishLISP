
; 2025-11-12 	rewrite with foreach, tested ok
;				use to set dimension base range
; what is this?
(defun c:4rc (/ dist pt1 pt2 pt3 pt4 minx maxx miny maxy os)
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
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(setvar "cmdecho" 0)
  
	(foreach n (list 0 1 2 3)
		(setq
			pt1 (list (- minx (* n dist)) (- miny (* n dist)))
			pt2 (list (+ maxx (* n dist)) (- miny (* n dist)))
			pt3 (list (+ maxx (* n dist)) (+ maxy (* n dist)))
			pt4 (list (- minx (* n dist)) (+ maxy (* n dist)))
        )
		(cmd "pline" pt1 pt2 pt3 pt4 "c")
    )

  	(setvar "cmdecho" 1)
	(setvar "osmode" os)
	(princ)
)

(princ "loaded. Start as C:4RC ")
(princ)