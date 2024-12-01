; what is this?
(defun c:4rc (/ pt1 pt2 pt3 pt4 minx maxx miny maxy rc1pt1 rc1pt2 rc1pt3 rc1pt4 rc2pt1 rc2pt2 rc2pt3 rc2pt4 rc3pt1 rc3pt2 rc3pt3 rc3pt4 rc4pt1 rc4pt2 rc4pt3 rc4pt4 os)
	(setq 
		pt1 (getpoint "\nUpper: ")
		dist (* (getvar "dimscale") 10)
		dist (if (zerop dist) 1000 dist)
	)
	(if pt1 (setq pt2 (getpoint "\nLower: ")))
	(if pt2 (setq pt3 (getpoint "\nLeft: ")))
	(if pt3 (setq pt4 (getpoint "\nRight: ")))

	(if pt4
		(progn
			(setq 
				minx (car pt3)
				maxx (car pt4)
				miny (cadr pt2)
				maxy (cadr pt1)

				rc1pt1 (list minx miny)
				rc1pt2 (list maxx miny)
				rc1pt3 (list maxx maxy)
				rc1pt4 (list minx maxy)

				rc2pt1 (list (- minx 1000) (- miny 1000))
				rc2pt2 (list (+ maxx 1000) (- miny 1000))
				rc2pt3 (list (+ maxx 1000) (+ maxy 1000))
				rc2pt4 (list (- minx 1000) (+ maxy 1000))

				rc3pt1 (list (- minx 2000) (- miny 2000))
				rc3pt2 (list (+ maxx 2000) (- miny 2000))
				rc3pt3 (list (+ maxx 2000) (+ maxy 2000))
				rc3pt4 (list (- minx 2000) (+ maxy 2000))

				rc4pt1 (list (- minx 3000) (- miny 3000))
				rc4pt2 (list (+ maxx 3000) (- miny 3000))
				rc4pt3 (list (+ maxx 3000) (+ maxy 3000))
				rc4pt4 (list (- minx 3000) (+ maxy 3000))

				os (getvar "osmode")
			);setq
			(setvar "osmode" 0)
			(setvar "cmdecho" 0)
			;(cmd "line" rc1pt1 rc1pt2 rc1pt3 rc1pt4 "c")
			(cmd "pline" rc2pt1 rc2pt2 rc2pt3 rc2pt4 "c")
			(cmd "pline" rc3pt1 rc3pt2 rc3pt3 rc3pt4 "c")
			(cmd "pline" rc4pt1 rc4pt2 rc4pt3 rc4pt4 "c")
			(setvar "cmdecho" 1)
			(setvar "osmode" os)
		);progn
	);if pt2
	(princ)
)

(princ "loaded. Start as C:4RC ")
(princ)