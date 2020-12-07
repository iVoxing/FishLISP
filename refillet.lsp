; FishLISP C:REFILLET
; Refillet two lines with new radius
; Usage:
;	Pick an ARC object attaching to two lines
;	Enter an new radius value
;	Done

; 2020-12-04 v1.0	

(defun c:refillet (/ en ent cen rad pt1 pt2 ls1 ls2 le1 le2 new_rad ce)
	(setq en (entsel "\nSelect ARC:"))
	(if en
		(progn
			(setq 
				ent (entget (car en))
				cen (cdr (assoc 10 ent))
				rad (cdr (assoc 40 ent))
				pt1 (polar cen (cdr (assoc 50 ent)) rad)
				pt2 (polar cen (cdr (assoc 51 ent)) rad)
				ls1 (ssget "x" (list '(-4 . "<and") '(0 . "line") '(-4 . "<or")  (cons 10 pt1) (cons 11 pt1) '(-4 . "or>") '(-4 . "and>")))
				ls2 (ssget "x" (list '(-4 . "<and") '(0 . "line") '(-4 . "<or")  (cons 10 pt2) (cons 11 pt2) '(-4 . "or>") '(-4 . "and>")))
				le1 (if ls1 (ssname ls1 0) nil)
				le2 (if ls2 (ssname ls2 0) nil)
			)
			(setq new_rad (getreal (strcat "\nNew radius: <" (rtos rad) ">")))
			(setq new_rad (if new_rad new_rad rad))
			(if (and le1 le2)
				(progn
					(setq ce (getvar "cmdecho"))
					(setvar "cmdecho" 0)
					(entdel (car en))
					(setvar "filletrad" new_rad)
					(cmd "fillet" le1 le2)
					(setvar "cmdecho" ce)
					(redraw)
				)
				(progn
					(princ "\nLines select error.")
				)
			)
		)
	)
	(princ)
)

(princ "loaded. Start as C:REFILLET.")
(princ)