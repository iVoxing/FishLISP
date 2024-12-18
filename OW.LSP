; 偏移Line，并修改与之相连的其他Line。
; 最新版本：3.0 7-28-1998。
; 版本简述：
;	1.0	只能修改正交的Line；Line仅限于Wall层。
;	2.0	能够处理非正交的Line；Line仅限于Wall层。
;	2.1	加快了选择速度。
;	2.2	增加了排除重复线的功能。
;	3.0	Line可处于任何图层。
;	3.1	把Getreal改成了Getdist。
;	4.0	可以正确运行于UCS下。
;   4.1 修正了仅有一条线时出现的错误。

(defun c:ow (/ olderr os hl mv_l dist loop en0 ent0 typ0 lay0 pt1 pt2 pt3 pt4 en1 ent1 ss0)
	(prompt "\nFishLISP. 7-17-1998. ")
	(prompt "\nOffset and modify line. Ver 4.1. ")
	(setq olderr *error*)
	(defun *error* (s)
		(setvar "osmode" (if os os 167))
		(setvar "cmdecho" 1)
		(setvar "highlight" (if hl hl 1))
		(if ucsf (setvar "ucsfollow" ucsf))
		(if ept (setvar "expert" ept))
		(setvar "cmdecho" 0)
		(cmd "ucs" "r" "ow_temp")
		(cmd "ucs" "d" "ow_temp")
		(setvar "cmdecho" 1)
		(setq *error* olderr olderr nil)
		(princ)
	)

	(defun mv_l (ps1 ps2 ps3 pt_1 pt_2 pt_3 / ss1 amt idx ent_l pt_4 pt_5 ang1 ang2 ang)
		(setvar "cmdecho" 0)
		(cmd "select" ss0 "")
		(setq ss1 (ssget "p" (list (cons ps1 pt_1))))
		(setq amt (if ss1 (sslength ss1) 0) idx 0)
		(repeat amt
			(setq 
				ent_l (entget (ssname ss1 idx))
				pt_4 (cdr (assoc ps2 ent_l))
				ang1 (angle pt_1 pt_3)
				ang2 (angle pt_1 pt_4)
				ang(- (/ pi 2) ang2 (- ang1))
			)
			(if (= (abs (sin (- ang1 ang2))) 1.0)
				(princ "\n1 repeated/joined line ignored. ")
				(progn
					(setq pt_5 (polar pt_1 ang2 (/ dist (sin ang))))
					(entmod (subst (cons ps1 pt_5) (assoc ps1 ent_l) ent_l))
					(entmod (setq ent1 (subst (cons ps3 pt_5) (assoc ps3 ent1) ent1)))
				)
			)
			(setq idx (1+ idx))
		)
	)

	(if ofwl_dist nil (setq ofwl_dist 0.0))
	(princ "\nOffset distance: <")
	(princ ofwl_dist)
	(setq dist (getdist "> "))
	(if dist nil (setq dist ofwl_dist))
	(setq ofwl_dist dist)

	(setq ucsf (getvar "ucsfollow"))
	(setq ept (getvar "expert"))
	(setq os (getvar "osmode"))
	(setq hl (getvar "highlight"))
	(setvar "ucsfollow" 0)
	(setvar "expert" 4)
	(setvar "osmode" 0)
	(setvar "cmdecho" 0)
	(setvar "highlight" 1)
	(cmd "ucs" "s" "ow_temp")
	(cmd "ucs" "w")

	(setq loop t)
	(while loop
		(setq en0 (entsel "\nSelect line to offset: "))
		(if en0
			(setq 
				ent0 (entget (car en0))
				typ0 (cdr (assoc 0 ent0))
				lay0 (cdr (assoc 8 ent0))
				pt1 (cdr (assoc 10 ent0))
				pt2 (cdr (assoc 11 ent0))
			)
		)
		(cond
			((not en0) (setq loop nil))
			((/= typ0 "LINE") (princ "\n1 was not Line. "))
			(t
				(setvar "cmdecho" 0)
				(fl_undo_begin)
				(princ "\nSide to offset? ")
				(cmd "offset" dist en0 pause "")
				(entdel (car en0))
				(setq 
					en1 (entlast)
					ent1 (entget en1)
					pt3 (cdr (assoc 10 ent1))
					pt4 (cdr (assoc 11 ent1))
					ss0 (ssget "f" (list pt1 pt2))
					ss0 (ssget "p" (list '(0 . "line") (cons 8 lay0)))
				)
				(if ss0
					(progn
						(mv_l 10 11 10 pt1 pt2 pt3)
						(mv_l 11 10 10 pt1 pt2 pt3)
						(mv_l 10 11 11 pt2 pt1 pt4)
						(mv_l 11 10 11 pt2 pt1 pt4)
					)
				)
				(fl_undo_end)
			)
		)
	)

	(setvar "osmode" os)
	(cmd "ucs" "r" "ow_temp")
	(cmd "ucs" "d" "ow_temp")
	(setvar "cmdecho" 1)
	(setvar "highlight" hl)
	(setvar "ucsfollow" ucsf)
	(setvar "expert" ept)
	(setq *error* olderr)
	(princ)
)

(princ "loaded. Start as C:OW ")
(princ)
