;
; FishLISP C:ET		Extend or trim the other side of lines
; 2025-11-25		rewriting
; 1998-09-24 v4.3	Error function.
; 1998-01-08 V4.2	Set OSMODE to none while edit
; 1997-12-18 V4.1	Now can edit in UCS
; 1996-12-17

(defun et_et ()
	(repeat cnt
		(setq 
			lin (entget (ssname ss n))
			l_pt1 (cdr (assoc 10 lin))
			l_pt2 (cdr (assoc 11 lin))
			int_lb (inters l_pt1 l_pt2 b_pt1 b_pt2)
			int_lf (inters l_pt1 l_pt2 f_pt1 f_pt2)
		)
		(if int_lb 
			(et_tri)
			(progn
				(setq int_lb (inters l_pt1 l_pt2 b_pt1 b_pt2 nil))
				(et_tri)
			)
		)
		(setq n (1+ n))
	)
)

(defun et_tri ()
	(if (> (distance l_pt1 int_lf) (distance l_pt1 int_lb))
		(entmod (subst (cons 10 int_lb) (cons 10 l_pt1) lin))
		(entmod (subst (cons 11 int_lb) (cons 11 l_pt2) lin))
	)
)

(defun et_ext ()
	(if (> (distance l_pt1 int_lb) (distance l_pt2 int_lb))
		(entmod (subst (cons 11 int_lb) (cons 11 l_pt2) lin))
		(entmod (subst (cons 10 int_lb) (cons 10 l_pt1) lin))
	)
)

(defun c:et (/ olderr ucsf osmd ens bd line_ent b_pt1 b_pt2 f_pt1 f_pt2 ss cnt n l_pt1 l_pt2 int_lb int_lf int_bf)
	(setvar "cmdecho" 0)
	(fl_undo_begin)
	(setq ucsf (getvar "ucsfollow"))
	(setq osmd (getvar "osmode"))
	(setvar "ucsfollow" 0)
	(setvar "osmode" 0)
	(cmd "_.ucs" "w")
	(setq ens (entsel "\nPick the boundary line to edit:"))
	(if (and 
			ens
			(= (setq bd (cdr (assoc 0 (setq line_ent (entget (car ens)))))) "LINE")
		)
		(progn
			(setq 
				b_pt1 (cdr (assoc 10 line_ent))
				b_pt2 (cdr (assoc 11 line_ent))
				f_pt1 (getpoint "\nDraw a fence to select lines:")
			)
		)
		(if bd (princ "\n1 was not Line."))
	)
	(if f_pt1
		(progn
			(initget 32)
			(setq f_pt2 (getpoint f_pt1 "\nSecond point of fence: "))
		)
	)
	(if f_pt2
		(setq ss (ssget "f" (list f_pt1 f_pt2)))
	)
	(if (and ss (not (setq int_bf (inters b_pt1 b_pt2 f_pt1 f_pt2))))
		(progn
			(setq 
				ss 	(ssget "p" '((0 . "LINE")))
				cnt (sslength ss)
				n	0
				bd 	nil
			)
			(et_et)
		)
		(cond 
			((and (not ss) (not bd)))
			( int_bf (princ "ERROR: Fence cannot cross the boundary."))
		)
	)
	(cmd "_.ucs" "p")
	(setvar "ucsfollow" ucsf)
	(setvar "osmode" osmd)
	(setvar "cmdecho" 0)
	(fl_undo_end)
	(setvar "cmdecho" 1)
	(princ)
)

(princ "loaded. Start as C:ET ")
(princ)