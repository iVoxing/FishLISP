; 画一扇平面推拉门
; 2004-05-13
; 2001-03-15

(defun c:lm (/ pt1 pt2 pt pt_x pt_y x_id y_id seg dist ds cl ucsf os pw)

	(setq lm_seg (if lm_seg lm_seg 4))
	(setq pt1 (getpoint "\nStart point: "))
	(if pt1 (setq pt2 (getpoint pt1 "\nEnd point: ")))
	(if pt2
		(progn
			(setq seg (getint (strcat "\nSegment: <" (rtos lm_seg 2 0) ">: ")))
			(setq seg (if seg seg lm_seg))
			(setq lm_seg seg)
			(setq dist (distance pt1 pt2))
			(setq dwid (if (< dist 100) 0.04 40.0))
			(setq ucsf (getvar "ucsfollow"))
			(setvar "ucsfollow" 0)
			(setvar "cmdecho" 0)
			(cmd "ucs" "3" pt1 pt2 "")
			(setq cl (getvar "clayer"))
			(if (tblsearch "layer" FLLT_DOOR)
				(setvar "clayer" FLLT_DOOR)
				(cmd "layer" "m" FLLT_DOOR "")
			)
			(setq os (getvar "osmode"))
			(setvar "osmode" 0)
			(setq pw (getvar "plinewid"))
			(setvar "plinewid" 0)
			(setq pt1 (trans pt1 0 1))
			(cmd "pline" pt1)
			(setq x_id 0.5 y_id 0 ds (/ dist seg))
			(repeat (* seg 2)
				(setq
					pt_x (* (fix x_id) ds)
					pt_y (* (expt -1 (fix y_id)) dwid)
					pt (list pt_x pt_y 0.0)
				)
				(cmd pt)
				(setq
					x_id (+ 0.5 x_id)
					y_id (+ 0.5 y_id)
				)
			)
			(setq pt2 (trans pt2 0 1))
			(cmd pt2 "c")
			(cmd "ucs" "p")
			(setvar "clayer" cl)
			(setvar "ucsfollow" ucsf)
			(setvar "osmode" os)
			(setvar "plinewid" pw)
		)
	)

	(princ)
)

(princ "loaded. Start as C:LM ")
(princ)
