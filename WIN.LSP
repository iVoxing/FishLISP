
(defun pt_sc (x_ y_ sc_)
	(append
		(list
			(+ (car x_) (* (- (car y_) (car x_)) sc_))
			(+ (cadr x_) (* (- (cadr y_) (cadr x_)) sc_))
		)
		(list 0.0)
	)
)

(defun in_win (op_ss_ / l1 l2 pt1 pt2 pt3 pt4 ptm1 ptm2 os oo ent wd /len sc1 sc2)
	(setq 
		l1 (entget (ssname op_ss_ 0))
		l2 (entget (ssname op_ss_ 1))
		pt1 (cdr (assoc 10 l1))
		pt2 (cdr (assoc 11 l1))
		len (distance pt1 pt2)
		sc1 (/ (- len 80) (* 2 len))
		sc2 (- 1 sc1)
		ptm1 (pt_sc pt1 pt2 sc1)
		ptm3 (pt_sc pt1 pt2 sc2)
		pt3 (cdr (assoc 10 l2))
		pt4 (cdr (assoc 11 l2))
		ptm2 (pt_sc pt3 pt4 sc1)
		ptm4 (pt_sc pt3 pt4 sc2)
	)

;--------------------- Add Xdata -------------------------------
; (setq ent (entget (entlast)))
;
; (if (tblsearch "appid" "fish_wd")
; nil
; (regapp "fish_wd")
; )
; (setq wb (list -3 (list "FISH_WD" (cons 1000 "win_base"))))
; (entmod (append ent (list wb)))
;----------------------------------------------------------------

	(setvar "clayer" FLLT_WINDOW)
	(if (inters pt1 pt3 pt2 pt4)
		(mapcar 'fl_make_line (list pt1 pt2 ptm1 ptm3) (list pt4 pt3 ptm4 ptm2))
		(mapcar 'fl_make_line (list pt1 pt2 ptm1 ptm3) (list pt3 pt4 ptm2 ptm4))
	)

)

(defun c:win (/ op_ss)
	(prompt "\nSelect frame line to draw:")
	(setq op_ss (ssget (list (cons 8 FLLT_WALL) '(0 . "LINE"))))
	(if (= (sslength op_ss) 2)
		(in_win op_ss)
		(princ "\nSelect error.")
	)
	(princ)
)

