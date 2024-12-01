; FishLISP
; C:Brm	break line and draw a mark with default value
; v3.1	2020-11-19	改写部分代码
; v3.0				解决了ucs问题

(defun brm (/ wid pt0 pt1 pt2 pt3 pt4 pt5 ang1 ang2)
	(setq 
		wid		(/ (getvar "dimscale") 2.0)
		pt1		(cadr en)
		pt3		(cdr (assoc 10 ent))
		pt4		(cdr (assoc 11 ent))
		ang1	(angle pt3 pt4)
		ang2	(+ ang1 (/ pi 2.0))
		pt2		(polar pt1 ang2 wid)
		pt0		(inters pt1 pt2 pt3 pt4 nil)
		pt1		(polar pt0 ang1 (* wid -1.5))
		pt2		(polar pt0 ang1 (* wid 1.5))
		pt3		(polar (polar pt0 ang1 (* wid -0.5)) ang2 (* wid 3.0))
		pt4		(polar (polar pt0 ang1 (* wid 0.5))  ang2 (* wid -3.0))
		pt5		(cdr (assoc 11 ent))
	)

	(entmod	(subst (cons 11 pt1) (assoc 11 ent) ent))
	(mapcar '(lambda (pt_list)
			(entmake (list '(0 . "line") (cons 8 lay) (cons 10 (car pt_list)) (cons 11 (cadr pt_list))))
			(list (list pt2 pt5) (list pt1 pt4) (list pt4 pt3) (list pt3 pt2))
	))
)

(defun c:bm (/ en ent lay)
	(prompt "\nBreak mark v3.0, Dec.17.1996")
	(setvar "blipmode" 0)
	(cssave)
	(setq en (entsel "\nSelect line to edit:"))
	(if (and en
			(= (setq lin (cdr (assoc 0 (setq ent (entget (car en)))))) "LINE")
			(< (setq lok (cdr (assoc 70 (tblsearch "layer" (setq lay (cdr (assoc 8 ent))))))) 68)
		)
		(brm)
		(cond
			((not en) (princ))
			((/= lin "LINE") (princ "\n1 was not LINE."))
			((>= lok 68) (princ "\n1 was on a locked layer."))
		) 
	)
	(csrestore)
	(setvar "blipmode" 1)
	(princ)
)
