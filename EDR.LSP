; 删除Door，并且恢复墙线。
; 2025-11-27	New block system, base width is 1
; 2025-11-20	重构
; 2023-10-06	l58 开始，好像可以重写

(defun is_dr_legal (en_name_ / ent)
	(and
		en_name_
		(= "INSERT" (cdr (assoc 0 (setq ent (entget en_name_)))))
		(member (strcase (cdr (assoc 2 ent))) (list "DR01" "DR02" "DR03" "DR04" "DR05" "DR06" "DR07" "DR08"))
	)
)

(defun get_renew (pt_ / en ent pt1 pt2 npt)
	(setq 
		en (ssname (ssget "x" (list (cons 8 FLLT_WALL) '(-4 . "<OR") (cons 10 pt_) (cons 11 pt_) '(-4 . "OR>"))) 0)
		ent (entget en)
		pt1 (cdr (assoc 10 ent))
		pt2 (cdr (assoc 11 ent))
		npt (if (< (distance pt_ pt1) (distance pt_ pt2)) pt2 pt1)
	)
	(list en npt)
)

(defun c:edr (/ olderr en enn ent pt0 ang wid ptm1 ptm2 ptf1 ptf2 ss l1 lent1 pt11 pt12 pt21 pt22 idx renew_list del_list newpt_list)
	(setvar "cmdecho" 0)
	(setq 	en (entsel "\nSelect a door to earse: ")
			enn (car en)
	)
	(if (is_dr_legal enn)
		(setq
			ent (entget enn)
			pt0 (cdr (assoc 10 ent))
			ang (cdr (assoc 50 ent))
			wid (abs (/ (cdr (assoc 41 ent)) 2))
			ptm1 (polar pt0 ang wid)
			ptm2 (polar pt0 ang (- wid))
			ptf1 (polar pt0 ang (* wid 1.005))
			ptf2 (polar pt0 ang (* wid -1.005))
			ss (ssget "f" (list ptf1 ptf2) (list (cons 8 FLLT_WALL)))
		)
		(princ "\n1 was not DOOR. ")
	)
	(if ss
		(progn 
			(entdel enn)
			(setq 
				l1 (ssname ss 0)
				lent1 (entget l1)
				pt11 (cdr (assoc 10 lent1))
				pt12 (cdr (assoc 11 lent1))
				wid1 (/ (distance pt11 pt12) 2.0)
				ang1 (angle pt11 pt12)
				pt11 (polar ptm1 ang1 wid1)
				pt12 (polar ptm1 ang1 (- wid1))
				pt21 (polar ptm2 ang1 wid1)
				pt22 (polar ptm2 ang1 (- wid1))
			)
			(if (inters pt11 pt21 pt12 pt22)
				(setq temp pt11 pt11 pt12 pt12 temp temp nil)
			)
			(setq idx 0)
			(repeat (sslength ss)
				(entdel (ssname ss idx))
				(setq idx (1+ idx))
			)
			(setq
				renew_list 	(mapcar 'get_renew (list pt11 pt12 pt21 pt22))
				del_list 	(mapcar 'car renew_list)
				newpt_list 	(mapcar 'cadr renew_list)
			)
			(mapcar 'entdel del_list)
			(mapcar 
				'(lambda (n_)
					(entmake (list '(0 . "LINE") (cons 8 FLLT_WALL) (cons 10 (nth n_ newpt_list)) (cons 11 (nth (+ n_ 2) newpt_list))))
				)
				'(0 1)
			)
		)
		(princ "\n1 DOOR was not in the Wall. ")
	)
	(princ)
)

(princ "loaded. Start as C:EDR ")
(princ)
