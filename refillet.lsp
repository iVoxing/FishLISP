; FishLISP C:REFILLET
; Refillet two lines with new radius
; Usage:
;	Pick an ARC object attaching to two lines
;	Enter an new radius value
;	Done
; 2025-11-24	rewrited, tested ok
; 2020-12-04 v1.0

(defun get_new_center (arc_en_ new_rad_ / ent old_cen hlf_ang cen_ang rad_dlt cen_dis)
	(setq
		ent (entget arc_en_)
		old_cen (cdr (assoc 10 ent))
		sta_ang (cdr (assoc 50 ent))
		hlf_ang (/ (- (cdr (assoc 51 ent)) sta_ang) 2.0)
		cen_ang (+ hlf_ang sta_ang)
		rad_dlt (- new_rad_ (cdr (assoc 40 ent)))
		cen_dis (princ (/ rad_dlt (cos hlf_ang) -1.0))
	)
	(polar old_cen cen_ang cen_dis)
)

(defun get_attach_line (pt_)
	(ssget "x" 
		(list 
			'(-4 . "<AND")
				'(0 . "LINE")
				'(-4 . "<OR")
					(cons 10 pt_)
					(cons 11 pt_)
				'(-4 . "OR>")
			'(-4 . "AND>")
		)
	)
)

(defun get_inters_by_en (en1_ en2_ / ent1 ent2)
	(setq
		ent1 (entget en1_)
		ent2 (entget en2_)
	)
	(inters
		(cdr (assoc 10 ent1))
		(cdr (assoc 11 ent1))
		(cdr (assoc 10 ent2))
		(cdr (assoc 11 ent2))
		nil
	)
)

(defun renew_arc (cen_ rad_ arc_ / ent)
	(setq
		ent (entget arc_)
		ent (subst (cons 10 cen_) (assoc 10 ent) ent)
		ent (subst (cons 40 rad_) (assoc 40 ent) ent)
	)
	(entmod ent)
)

(defun renew_attach_line (pt_ line_ inter_ / pt1 pt2)
	(setq
		ent (entget line_)
		pt1 (cdr (assoc 10 ent))
		pt2 (cdr (assoc 11 ent))
	)
	(if (> (distance inter_ pt1) (distance inter_ pt2))
		(entmod (subst (cons 11 pt_) (assoc 11 ent) ent))
		(entmod (subst (cons 10 pt_) (assoc 10 ent) ent))
	)
)

(defun refillet (/ en enn ent cen rad ap1 ap2 pt1 pt2 ls1 ls2 le1 le2 new_rad typ ty1 ty2 interpt cen2 new_pt1 new_pt2)
	;(setvar "osmode" 0)
	(setq en (entsel "\nSelect ARC:"))
	(if en
		(setq
			enn (car en)
			ent (entget enn)
			typ (cdr (assoc 0 ent))
		)
	)
	(if (= typ "ARC")
		(setq
			cen (cdr (assoc 10 ent))
			rad (cdr (assoc 40 ent))
			new_rad (getreal (strcat "\nNew radius: <" (rtos rad) ">"))
		)
	)
	(if new_rad
		(setq
			ap1 (assoc 50 ent)
			ap2 (assoc 51 ent)
			pt1 (polar cen (cdr ap1) rad)
			pt2 (polar cen (cdr ap2) rad)
			ls1 (get_attach_line pt1)
			ls2 (get_attach_line pt2)
			le1 (ssname ls1 0)
			le2 (ssname ls2 0)
		)
	)
	(if (and le1 le2)
		(setq
			ty1 (cdr (assoc 0 (entget le1)))
			ty2 (cdr (assoc 0 (entget le2)))
		)
	)
	(if (= ty1 ty2 "LINE")
		(progn
			(setq
				interpt (get_inters_by_en le1 le2)
			 	cen2 (get_new_center enn new_rad)
			 	new_pt1 (polar cen2 (cdr ap1) new_rad)
			 	new_pt2 (polar cen2 (cdr ap2) new_rad)
			)
			(renew_arc cen2 new_rad enn)
			(renew_attach_line new_pt1 le1 interpt)
			(renew_attach_line new_pt2 le2 interpt)
		)
	)
	(princ)
)

(defun c:ref ()
	(refillet)
	(princ)
)

(princ "loaded. C:REF.")
(princ)