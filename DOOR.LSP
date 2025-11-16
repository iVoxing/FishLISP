; 2025-11-12 rewrite, test ok

; Description of varibles
; l1 l2	two line to contain the door
; pt1 pt2	end points of l1
; pt3 pt4	end points of l2
; ptm1 ptm2	mid points of l1 and l2
; ptm		mid point of ptm1 and ptm2, the base point to insert

(defun in_door (line_ss / ssdr ptm1 ptm2 os oo cl ptm sc ang dr)
	(setq os (getvar "osmode"))
	(setq oo (getvar "orthomode"))
	(setq cl (getvar "clayer"))
	(setvar "cmdecho" 0)
	(setvar "osmode" 0)
	(setvar "orthomode" 0)
	(if (tblsearch "layer" FLLT_DOOR)
		(setvar "clayer" FLLT_DOOR)
		(cmd "layer" "m" FLLT_DOOR "")
	)
  
	(setq ptm_list 
        (mapcar 
        	'(lambda (x / line_ent pt1 pt2)
				(setq line_ent (entget (ssname op_ss x))
					pt1 (cdr (assoc 10 line_ent))
					pt2 (cdr (assoc 11 line_ent))
				)
				(append (list (/ (+ (car pt1) (car pt2)) 2.0) (/ (+ (cadr pt1) (cadr pt2)) 2.0)) (list 0.0))
            )
			(list 0 1)
		)
	)
  
	(setq
		ptm1 (car ptm_list)
        ptm2 (cadr ptm_list)
		ptm (append (list (/ (+ (car ptm1) (car ptm2)) 2.0) (/ (+ (cadr ptm1) (cadr ptm2)) 2.0)) (list 0.0))
		sc (/ (distance ptm1 ptm2) 1000.0)
		ang (/ (* (angle ptm1 ptm2) 180.0) pi)
	)
  
  	(defun ins_doors (door_name)
		(cmd "insert" door_name ptm sc sc ang)
		(setq ssdr (ssadd (entlast) ssdr))
    )
  
	(defun clr_doors (/ dr)
		(setq dr (entsel "\nSelect the door:"))
		(if dr
			(progn
				(setq dr (car dr))
				(cmd "erase" ssdr "r" dr "")
			)
		)
    )
  
  	(setq ssdr (ssadd))
  	(cond
   		((and (>= sc 1.001) (< sc 2.3))
			(mapcar 'ins_doors (list "dr1" "dr2"))
			(clr_doors)
		)
		((< sc 1.001)
			(mapcar 'ins_doors (list "dr3" "dr4" "dr5" "dr6"))
			(clr_doors)
		)
		((>= sc 2.3)
			(mapcar 'ins_doors (list "dr7" "dr8"))
			(clr_doors)
		)
	)
	(setvar "cmdecho" 1)
	(setvar "osmode" os)
	(setvar "orthomode" oo)
	(setvar "clayer" cl)
)

(defun c:door (/ olderr ucsf op_ss )

	(setq olderr *error*)
	(defun *error* (s)
		(setvar "cmdecho" 0)
		(fl_undo_end)
		(setq *error* olderr)
		(princ)
	)

	(setvar "cmdecho" 0)
	(fl_undo_begin)
	(setq ucsf (getvar "ucsfollow"))
	(setvar "ucsfollow" 0)
	(setvar "expert" 4)
	(cmd "ucs" "s""$temp$")
	(cmd "ucs" "w")

	(prompt "\nSelect frame line to draw:")
	(setq op_ss (ssget (list (cons 8 (strcat FLLT_WALL "," FLLT_DOOR "," FLLT_COL)) '(0 . "line"))))
	(if (= (sslength op_ss) 2)
		(in_door op_ss )
		(princ "\nSelect error.")
	)

	(setvar "cmdecho" 0)
	(cmd "ucs" "r" "$temp$")
	(cmd "ucs" "d" "$temp$")
	(setvar "ucsfollow" ucsf)
	(fl_undo_end)
	(setvar "expert" 0)
	(setvar "cmdecho" 1)

	(princ)
)

