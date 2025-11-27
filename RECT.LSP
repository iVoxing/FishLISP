; ¸üÁé»îµÄ»­Rectangle
; 2025-11-20	optimization, tested ok
; 1995-03-12

(defun make_line (pt1_ pt2_)
	(entmake (list '(0 . "LINE") (cons 10 (trans pt1_ 1 0)) (cons 11 (trans pt2_ 1 0))))
)

(defun line_box (/ pt1 pt2 pt3 pt4)
	(initget "Pline")
	(setq pt1 (getpoint "\nPline mode. <first corner>: "))
	(cond
		((listp pt1)
			(setq 
				pt3 (getcorner pt1 "\nThe other corner: ")
				pt2 (list (car pt1) (cadr pt3))
				pt4 (list (car pt3) (cadr pt1))
			)
			(make_line pt1 pt2)
			(make_line pt2 pt3)
			(make_line pt3 pt4)
			(make_line pt4 pt1)
		)
		((= pt1 "Pline")
			(setq RECTL nil)
			(pline_box)
		)
		(t)
	)
)

(defun pline_box (/ wid pt1 pt2 pt3 pt4)
	(setq PL_WIDTH (if PL_WIDTH PL_WIDTH (getvar "plinewid")))
	(initget "Line Width")
	(setq pt1 (getpoint (strcat "\nLine mode/Width[" (rtos PL_WIDTH) "]: <first corner>")))
	(cond
		((not pt1))
		((listp pt1)
			(setq 
				pt3 (getcorner pt1 "\nThe other corner: ")
				pt2 (list (car pt1) (cadr pt3))
				pt4 (list (car pt3) (cadr pt1))
			)
			(setvar "plinewid" PL_WIDTH)
			(fl_make_pline (list pt1 pt2 pt3 pt4) 1)
		)
		((= pt1 "Line")
			(setq RECTL 1)
			(line_box)
		)
		((= pt1 "Width")
			(setq wid (getreal (strcat "\nWidth: <" (rtos PL_WIDTH) ">: ")))
			(setq wid (if wid wid PL_WIDTH))
			(setq PL_WIDTH wid)
			(pline_box)
		)
		(t)
	)
)

(defun c:rect ()
	(if RECTL
		(line_box)
		(pline_box)
	)
	(princ)
)

; 2025-11-25 tested ok
(defun c:rct (/ s_ang oo os pt1 pt2 pt3 pt4 an1 an2)
	(setq 
		s_ang (getvar "snapang")
		oo (getvar "orthomode")
	)
	(setq pt1 (getpoint "\nPick a point: "))
	(if pt1 (setq pt2 (getpoint pt1 "\nPick a point: ")))
	(if pt2 
		(progn
			(setq an1 (angle pt2 pt1))
			(setvar "snapang" an1)
			(setvar "orthomode" 1)
			(setq pt3 (getpoint pt2 "\nPick a point: "))
		)
	)
	(if pt3
		(progn
			(setq 
				ds1 (distance pt1 pt2)
				pt4 (polar pt3 an1 ds1)
			)
			(fl_make_pline (list pt1 pt2 pt3 pt4) 1)
		)
	)
	(setvar "snapang" s_ang)
	(setvar "orthomode" oo)
	(princ)
)
 
(princ "loaded. C:RECT C:RCT")
(princ)

 