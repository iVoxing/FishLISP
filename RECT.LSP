; ¸üÁé»îµÄ»­Rectangle
; 2025-11-20	rewrite
; 1995-03-12

(defun lbx (/ pt1 pt2 pt3 pt4)
	(initget "Pline")
	(setq pt1 (getpoint "\nPline mode. <first corner>: "))
	(cond
		((listp pt1)
			(setq 
				pt3 (getcorner pt1 "\nThe other corner: ")
				pt2 (list (car pt1) (cadr pt3))
				pt4 (list (car pt3) (cadr pt1))
			)
			(setvar "osmode" 0)
			(cmd "line" pt1 pt2 pt3 pt4 "c")
		)
		((= pt1 "Pline")
			(setq rectl nil)
			(pbx)
		)
		(t)
	)
) 

(defun pbx (/ wid pt1 pt2 pt3 pt4) 
	(setq PL_WIDTH (if PL_WIDTH PL_WIDTH (getvar "plinewid")))
	(initget "Line Width")
	(setq pt1 (getpoint (strcat "\nLine mode/Width[" PL_WIDTH "]: <first corner>")))
	(cond
		((not pt1))
		((listp pt1)
			(setq 
				pt3 (getcorner pt1 "\nThe other corner: ")
				pt2 (list (car pt1) (cadr pt3))
				pt4 (list (car pt3) (cadr pt1))
			)
			(setvar "osmode" 0)
			(cmd "pline" pt1 "w" PL_WIDTH PL_WIDTH pt2 pt3 pt4 "c")
		)
		((= pt1 "Line")
			(setq rectl 1)
			(lbx)
		)
		((= pt1 "Width")
			(setq wid (getreal (strcat "\nWidth: <" PL_WIDTH ">: ")))
			(setq wid (if wid wid PL_WIDTH))
			(setq PL_WIDTH wid)
			(pbx)
		)
		(t)
	)
) 

(defun c:rect (/ os olderr lbx pbx recthp rectul)
	(setq olderr *error*)
	(defun *error* (s)
		(if (= s "Function cancelled") nil
		(princ (strcat "\nError: " s))
		)
		(if os (setvar "osmode" os))
		(setq *error* olderr olderr nil)
		(setq lbx nil pbx nil recthp nil rectul nil)
		(princ)
	)


	(setq os (getvar "osmode"))
	(setvar "cmdecho" 0)
	(if rectl
		(lbx)
		(pbx)
	)
	(setvar "cmdecho" 1)
	(setvar "osmode" os)
	(setq *error* olderr)
	(princ)
)

(defun c:rct (/ s_ang oo os pt1 pt2 pt3 pt4 an1 an2)
	(setq 
		s_ang (getvar "snapang")
		oo (getvar "orthomode")
		os (getvar "osmode")
	)
	(setvar "osmode" 0)
	(setvar "cmdecho" 0)
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
			(cmd "_pline" pt1 pt2 pt3 pt4 "c")
		)
	)
	(setvar "snapang" s_ang)
	(setvar "orthomode" oo)
	(setvar "osmode" os)
	(setvar "cmdecho" 1)
	(princ)
)
 
(princ "loaded. C:RECT C:RCT")
(princ)

 