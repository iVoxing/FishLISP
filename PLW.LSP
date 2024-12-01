; History:
; 2003-07-013.0 合并pline和line的命令；完善程序
; 2.5 处理UCS
; 2001-10-082.0 处理CIRCLE等
;1995-11-271.0 最初版本

(defun plw_chw (pl_ss pl_wd / count elst)
	(setvar "cmdecho" 0)
	(setq count 0)
	(repeat (sslength pl_ss)
		(setq elst (ssname pl_ss count))
		(cmd "pedit" elst "w" pl_wd "")
		(setq count (1+ count))
	)
)

(defun plw_setwd ()
	(if plw_wd nil (setq plw_wd (getvar "plinewid")))
	(princ "\nEnter the new width<")
	(princ plw_wd)
	(setq wd (getreal ">:"))
	(if wd nil (setq wd plw_wd))
	(setq plw_wd wd)
)

(defun c:lw (/ wd ss ss_l ss_p count elst ent etyp d0 d1 d2 cen uf olderr)
	(setq olderr *error*)
	(defun *error* (s)
		(if uf (setvar "ucsfollow" uf))
		(setvar "highlight" 1)
		(setvar "cmdecho" 0)
		(cmd "ucs" "p")
		(cmd "_undo" "end")
		(setvar "cmdecho" 1)
		(setq *error* olderr olderr nil)
		(princ)
	)
	(princ "\nFishLISP. Nov 27 1995")
	(princ "\nChange lines Width, ver 1.3.")
	(setq uf (getvar "ucsfollow"))
	(setvar "cmdecho" 0)
	(setvar "ucsfollow" 0)
	(cmd "_undo" "group")

	(plw_setwd)

	(setq ss (ssget '((0 . "LINE,ARC,CIRCLE,POLYLINE,LWPOLYLINE"))))
	(cmd "ucs" "w")
	(setvar "highlight" 0)
	(if ss
		(progn
			(cmd "select" ss "")
			(setq ss_p (ssget "P" '((0 . "POLYLINE,LWPOLYLINE"))))
			(cmd "select" ss "")
			(setq ss_l (ssget "P" '((0 . "LINE,ARC,CIRCLE"))))
		)
	)

	(if ss_p (plw_chw ss_p wd))
	(while ss_l
		(setvar "cmdecho" 0)
		(setq 
			elst (ssname ss_l 0)
			ent (entget elst)
			etyp (cdr (assoc 0 ent))
		)
		(if (= etyp "CIRCLE")
			(progn
				(setq 
					d0 (* (cdr (assoc 40 ent)) 2.0)
					d1 (- d0 wd)
					d2 (+ d0 wd)
					cen (cdr (assoc 10 ent))
				)
				(cmd "donut" d1 d2 cen "")
				(entdel elst)
			)
			(cmd "pedit" elst "y""j" ss_l "" "w" wd "")
		)
		(setq ss_l (ssget "P" '((0 . "LINE,ARC,CIRCLE"))))
	)
	(setvar "highlight" 1)
	(setvar "cmdecho" 0)
	(cmd "ucs" "p")
	(setvar "ucsfollow" uf)
	(cmd "_undo" "end")
	(setvar "cmdecho" 1)
	(princ " done.")

	(princ)
)

(princ "loaded. Start as C:LW ")
(princ)
