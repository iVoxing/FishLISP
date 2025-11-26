; 尚未整理变量
; 已经可用
; 要不要加上Undo呢?
; 部分模块应该和Wall-1合并
;	2025-11-21	rewriting
;	1997-10-08

; ==== init ====

(setvar "cmdecho" 0)
(if (tblsearch "layer" "wall-base")
	nil 
	(cmd "_.layer" "n" "wall-base" "c" "r" "wall-base" "")
)
(if (tblsearch "layer" FLLT_WALL) 
	nil
	(cmd "n" FLLT_WALL "c" "41" FLLT_WALL "")
)
(setvar "cmdecho" 1)

; ==== function ====

(defun wall_err (s)
	(if (/= s "Function cancelled")
		(princ (strcat "\nError:" s))
	)
	(if grdmd (setvar "gridmode" grdmd))
	(if ucsf (setvar "ucsfollow" ucsf))
	(setvar "cmdecho" 0)
	(cmd "_.undo" "end")
	(setvar "cmdecho" 1)
	(setvar "expert" 0)
	(setq lpt0_1 nil segmnt nil)
	(if ai_beep (ai_beep))
	(redraw)
	(princ)
)

(defun wall_xc (ss_xc_ / amt en_lst int m ent1 ent2 en rpt pt1 pt2 pt1_1 pt1_2 pt2_1 pt2_2 l ent)
	(setq 
		amt (sslength ss_xc_)
		idx 0
		en_lst (list)
	)
	(repeat amt
		(setq 
			en (ssname ss_xc_ idx)
			en_lst (append en_lst (list en))
			idx (1+ idx)
		)
		(redraw en 2)
	)
	(setq idx 0)
	(repeat amt
		(setq 
			ent1 (entget (nth idx en_lst) '("FL_WALL"))
			pt1_1 (cdr (assoc 10 ent1))
			pt1_2 (cdr (assoc 11 ent1))
			m (1+ idx)
		)
		(if (= m amt) (setq m 0))
		(cmd "_.ucs" "e" (cdr (assoc -1 ent1)))
		(setq int_lst (list (car (trans pt1_1 0 1))))
		(repeat (1- amt)
			(setq 
				ent2 (entget (nth m en_lst))
				pt2_1 (cdr (assoc 10 ent2))
				pt2_2 (cdr (assoc 11 ent2))
				int (inters pt1_1 pt1_2 pt2_1 pt2_2)
			)
			(if (member int (list pt1_1 pt1_2)) 
				(setq int nil)
			)
			(if int 
				(setq int_lst (append int_lst (list (car (trans int 0 1)))))
			)
			(setq m (1+ m))
			(if (= m amt) 
				(setq m 0)
			)
		)
		(setq int_lst (append int_lst (list (car (trans pt1_2 0 1)))))
		(setq int_lst (vl-sort int_lst '<))
		(if (< (- (cadr int_lst) (car int_lst)) (* 3 FL_WALL_WIDTH))
			(setq int_lst (cdr int_lst))
		)
		(setq l 0 rpt (/ (length int_lst) 2))
		(repeat rpt
			(setq 
				pt1 (trans (list (nth l int_lst) 0) 1 0)
				pt2 (trans (list (nth (1+ l) int_lst) 0) 1 0)
				ent (subst (cons 10 pt1) (assoc 10 ent1) ent1)
				ent (subst (cons 11 pt2) (assoc 11 ent) ent)
			)
			(entmake ent)
			(setq l (+ l 2))
		)
		(setq idx (1+ idx))
	)
	(cmd "_.erase" ss_xc_ "")
	(redraw)
	(cmd "_.ucs" "r" "xc_temp")
) 

(defun draw_line (pt1_ pt2_ lay_ xdt_ / ent)
	(entmake (list '(0 . "LINE") (cons 8 lay_) (cons 10 pt1_) (cons 11 pt2_)))
	(setq ent (entget (entlast)))
	(if xdt_
		nil 
		(setq 
			wall_id (cdr (assoc 5 ent))
			xdt_ (list -3 (list "FL_WALL"
					(cons 1000 wall_id)
					(cons 1002 "{")
					(cons 1000 "Wall_base")
					(cons 1041 (* 2 dist))
					(cons 1002 "}")
				))
		)
	)
	(regapp "FL_WALL")
	(entmod (append ent (list xdt_)))
)

(defun make_line (mpt1_ mpt2_ / ang0 ang1 ang2)
	(if lpt1
		(setq lpt3 lpt1 lpt4 lpt2 rpt3 rpt1 rpt4 rpt2)
	)
	(setq 
		ang0 (angle mpt1_ mpt2_)
		ang1 (+ ang0 (/ pi 2))
		ang2 (- ang0 (/ pi 2))
		lpt1 (polar mpt1_ ang1 dist)
		lpt2 (polar mpt2_ ang1 dist)
		rpt1 (polar mpt1_ ang2 dist)
		rpt2 (polar mpt2_ ang2 dist)
	)

	(if lpt0_1
		(setq 
			lpt1 (modify_line lpt4 lpt3 lpt1 lpt2 11)
			rpt1 (modify_line rpt4 rpt3 rpt1 rpt2 11)
		)
		(setq lpt0_1 lpt1 lpt0_2 lpt2 rpt0_1 rpt1 rpt0_2 rpt2)
	)
	(if (= pt0 pt2)
		(setq 
			lpt2 (modify_line lpt0_1 lpt0_2 lpt1 lpt2 10)
			rpt2 (modify_line rpt0_1 rpt0_2 rpt1 rpt2 10)
		)
	)

	(setq ssXC (ssget "cp" (list lpt1 lpt2 rpt2 rpt1) (list (cons 8 FLLT_WALL))))
	(draw_line mpt1_ mpt2_ "wall-base" nil)
	(setq wl_xdt (list -3 (list "FL_WALL"
					(cons 1000 wall_id)
					(cons 1002 "{")
					(cons 1000 "Wall_line")
					(cons 1041 (* 2 dist))
					(cons 1002 "}")
				))
	)
	(draw_line lpt1 lpt2 FLLT_WALL wl_xdt)
	(if ssXC (setq ssXC (ssadd (entlast) ssXC)))
	(draw_line rpt1 rpt2 FLLT_WALL wl_xdt)
	(if ssXC (setq ssXC (ssadd (entlast) ssXC)))

	(if ssXC 
		(progn
			(wall_xc ssXC)
			(if (= (rem (sslength ssXC) 2) 0) nil (setq pt2 nil))
		)
	)

)

(defun modify_line (mdpt1_ mdpt2_ mdpt3_ mdpt4_ Etype_ / int ent)
	(setq 
		ent (entget (ssname (ssget "x" (list (cons 8 FLLT_WALL) (cons Etype_ mdpt1_))) 0))
		int (inters mdpt1_ mdpt2_ mdpt3_ mdpt4_ nil)
		ent (subst (cons Etype_ int) (assoc Etype_ ent) ent)
	)
	(entmod ent)
	int
)

(defun c:wall (/ grdmd ucsf dist pt0 pt1 pt2 Segmnt lpt1 lpt2 lpt3 lpt4 rpt1 rpt2 rpt3 rpt4 lpt0_1 lpt0_2 rpt0_1 rpt0_2)
	(princ "\nConvert Lines to Wall-lines. Start as C:Wall")
	(setq grdmd (getvar "gridmode"))
	(setq ucsf (getvar "ucsfollow"))
	(setvar "cmdecho" 0)
	(setvar "expert" 4)
	(setvar "ucsicon" 0)
	(cmd "_.ucs" "s" "xc_temp")
	(setvar "gridmode" 0)
	(setvar "ucsfollow" 0)

	; 缺省墙体厚度 200
	(setq FL_WALL_WIDTH (if FL_WALL_WIDTH FL_WALL_WIDTH 100.0))

	(setq dist (getint (strcat "\nWidth of the wall<" (rtos (* 2 FL_WALL_WIDTH) 2 1) ">: ")))
	(if dist
		(setq dist (/ dist 2.0))
		(setq dist FL_WALL_WIDTH)
	)
	(setq FL_WALL_WIDTH dist)

	(setq pt0 (getpoint "\nWall-line From point: ")
			pt1 pt0
			Segmnt 0
			lpt0_1 nil
	)
	(while pt1
		(if (> Segmnt 1) (initget "Close"))
		(setq pt2 (getpoint pt1 "\nTo point: "))
		(cond 
			((= pt2 "Close")
				(setq pt2 pt0)
				(make_line pt1 pt2)
				(setq pt1 nil)
			)
			((not pt2) (setq pt1 nil))
			((listp pt2) 
				(make_line pt1 pt2)
				(setq pt1 pt2)
			)
		)
		(if pt1 (setq Segmnt (1+ Segmnt)))
	)

	(cmd "_.ucs" "r" "xc_temp")
	(cmd "_.ucs" "d" "xc_temp")
	(setvar "ucsicon" 1)
	(setvar "expert" 0)
	(setvar "gridmode" grdmd)
	(setvar "ucsfollow" ucsf)
	(setvar "cmdecho" 1)
	(princ)
)

