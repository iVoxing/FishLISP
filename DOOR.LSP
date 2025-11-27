; insert DR0# in wall opening
; 2025-11-27	New block system, base width is 1
;				tested, ok
; 2025-11-12 rewrite, test ok

(defun ins_dr_block (dr_name_)
	(cmd "_insert" dr_name_ ptm sc sc ang)
	(setq DR_SS (ssadd (entlast) DR_SS))
)

(defun clr_doors (/ dr)
	(setq dr (entsel "\nSelect the door:"))
	(if dr
		(progn
			(setq dr (car dr))
			(cmd "_erase" DR_SS "r" dr "")
		)
	)
)

(defun in_door (line_ss_ / ptm1 ptm2 os oo cl ptm sc ang dr)
	(setq os (getvar "osmode"))
	(setq oo (getvar "orthomode"))
	(setq cl (getvar "clayer"))
	(setvar "cmdecho" 0)
	(setvar "osmode" 0)
	(setvar "orthomode" 0)
  
	(setq ptm_list 
        (mapcar 
        	'(lambda (x_ / line_ent pt1 pt2)
				(setq line_ent (entget (ssname line_ss_ x_))
					pt1 (cdr (assoc 10 line_ent))
					pt2 (cdr (assoc 11 line_ent))
				)
				(append (list (/ (+ (car pt1) (car pt2)) 2.0) (/ (+ (cadr pt1) (cadr pt2)) 2.0)) (list 0.0))
            )
			(list 0 1)
		)
	)
  
	(setq
		ptm1 	(car ptm_list)
        ptm2 	(cadr ptm_list)
		ptm 	(append (list (/ (+ (car ptm1) (car ptm2)) 2.0) (/ (+ (cadr ptm1) (cadr ptm2)) 2.0)) (list 0.0))
		sc 		(distance ptm1 ptm2)
		ang 	(/ (* (angle ptm1 ptm2) 180.0) pi)
	)
  
  	(setq DR_SS (ssadd))
  	(cond
   		((and (>= sc 1001) (< sc 2300))
			(mapcar 'ins_dr_block (list "DR01" "DR02"))
			(clr_doors)
		)
		((< sc 1001)
			(mapcar 'ins_dr_block (list "DR03" "DR04" "DR05" "DR06"))
			(clr_doors)
		)
		((>= sc 2300)
			(mapcar 'ins_dr_block (list "DR07" "DR08"))
			(clr_doors)
		)
	)
	(setvar "cmdecho" 1)
	(setvar "osmode" os)
	(setvar "orthomode" oo)
	(setvar "clayer" cl)
)

(defun c:door (/ olderr ucsf op_ss )
	(setvar "cmdecho" 0)
	(if (tblsearch "layer" FLLT_DOOR)
		(setvar "clayer" FLLT_DOOR)
		(cmd "_layer" "m" FLLT_DOOR "")
	)
	(setq ucsf (getvar "ucsfollow"))
	(setvar "ucsfollow" 0)
	(setvar "expert" 4)
	(cmd "_ucs" "s""$temp$")
	(cmd "_ucs" "w")

	(prompt "\nSelect frame line to draw:")
	(setq op_ss (ssget (list (cons 8 (strcat FLLT_WALL "," FLLT_DOOR "," FLLT_COL)) '(0 . "LINE"))))
	(if (= (sslength op_ss) 2)
		(in_door op_ss)
		(princ "\nSelect error.")
	)

	(setvar "cmdecho" 0)
	(cmd "_ucs" "r" "$temp$")
	(cmd "_ucs" "d" "$temp$")
	(setvar "ucsfollow" ucsf)
	(setvar "expert" 0)
	(setvar "cmdecho" 1)

	(princ)
)

