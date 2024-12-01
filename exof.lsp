(defun c:exof (/ pt1 pt2 dis0 dis1 dis2 ang1+ ang1- ang2+ pta ptb ptc ptd pw)
	(setq pt1 (getpoint "\nFirst point: "))
	(if pt1 (setq pt2 (getpoint pt1 "\nSecond point: ")))
	(if pt2
		(progn
			(setq 
				exof_dis1 (if exof_dis1 exof_dis1 300.0)
				exof_dis2 (if exof_dis2 exof_dis2 1200.0)
				dis0 (distance pt1 pt2)
				ang1+ (angle pt1 pt2)
				ang1- (angle pt2 pt1)
				ang2+ (+ ang1+ (/ pi 2))
			)
			(princ "\nExtend dist: <")
			(princ exof_dis1)
			(setq dis1 (getdist "> "))
			(setq dis1 (if dis1 dis1 exof_dis1))
			(princ "\nOffset dist: <")
			(princ exof_dis2)
			(setq 
				dis2 (getdist "> ")
				dis2 (if dis2 dis2 exof_dis2)
				pta (polar pt1 ang1- dis1)
				ptb (polar pta ang2+ dis2)
				ptd (polar pt2 ang1+ dis1)
				ptc (polar ptd ang2+ dis2)
				pw (getvar "plinewid")
			)
			(setvar "cmdecho" 0)
			(setvar "plinewid" 0)
			(cmd "pline" pta ptb ptc ptd "")
			(setq exof_dis1 dis1 exof_dis2 dis2)
			(setvar "plinewid" pw)
		);progn
	)
	(princ)
)

(princ)
