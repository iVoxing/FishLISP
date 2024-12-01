; 
; Completed appliction
; Select lines to convert to Wall-lines
; Problems:	1. How to create Walls in diffrent width.
;		2. How to create ARC Walls.
;		3. It's too slow double offsetting lines.


; ====== Loading time file check
;
(setq olderr *error*)
(defun *error* (s)
	(if ai_beep (ai_beep))
	(princ "\nFunction.lsp not found in search path.\nApplication abort. ")
	(setq *error* olderr olderr nil)
	(princ)
)
(if (findfile "function.lsp") (setq *error* olderr olderr nil) (exit))

; ====== Loading time layer check
;
(setvar "cmdecho" 0)
(if (zerop (getvar "handles")) (cmd "_.handles" "on"))
(setq clay (getvar "clayer"))
(if (tblsearch "layer" "wall-base") nil
	(cmd "_.layer" "n" "wall-base" "c" "y" "wall-base" "")
)
(if (tblsearch "layer" FLLT_WALL) nil
	(cmd "_.layer" "n" FLLT_WALL "c" "41" FLLT_WALL "")
)
(cmd "layer" "off" "wall-base" "")
(setvar "clayer" clay)
(setq clay nil)

; ====== Function: Offset specified lines to both side and modify the L-cross;
;

(defun l2w_ol (ss_ol / ss_xc idx ent ang ang1 ang2 pt1 pt2 pt3 pt4 pt5 pt6 ss_pt1 ss_pt2 entt angt dis ptt1 ptt2 ptt3 ptt4 ptt5 ptt6 int)
	(setvar "cmdecho" 0)
	(cmd "_.chprop" ss_ol "" "la" "wall-base" "")
	(setq 
		ss_xc (ssadd)
		idx 0
	)
	(repeat (sslength ss_ol)
		(setq 
			en (ssname ss_ol idx)
			ent (entget en)
			wall_id (cdr (assoc 5 ent))
			pt1 (cdr (assoc 10 ent))
			pt2 (cdr (assoc 11 ent))
			ang (angle pt1 pt2)
			ang1 (+ ang (/ pi 2))
			ang2 (- ang (/ pi 2))
			pt3 (polar pt1 ang1 dist)
			pt4 (polar pt2 ang1 dist)
			pt5 (polar pt1 ang2 dist)
			pt6 (polar pt2 ang2 dist)
			wb_xdt (list -3 (list "FISH_WALL"
						(cons 1000 wall_id)
						(cons 1002 "{")
						(cons 1000 "Wall_base")
						(cons 1041 (* 2 dist))
						(cons 1002 "}")
					))
			wl_xdt (list -3 (list "FISH_WALL"
						(cons 1000 wall_id)
						(cons 1002 "{")
						(cons 1000 "Wall_line")
						(cons 1041 (* 2 dist))
						(cons 1002 "}")
					))
		)
		(regapp "FISH_WALL")
		(entmod (append ent (list wb_xdt)))

		(setq ss_pt1 (ssdel en (ssget "x" (list (cons -4 "<or") (cons 10 pt1) (cons 11 pt1) (cons -4 "or>") '(8 . "wall-base")))))
		(if (> (sslength ss_pt1) 0)
		(progn
			(setq 
				entt (entget (ssname ss_pt1 0))
				ptt1 (cdr (assoc 10 entt))
				ptt2 (cdr (assoc 11 entt))
				ang (angle ptt1 ptt2)
				ang1 (+ ang (/ pi 2))
				ang2 (- ang (/ pi 2))
				ptt3 (polar ptt1 ang1 dist)
				ptt4 (polar ptt2 ang1 dist)
				ptt5 (polar ptt1 ang2 dist)
				ptt6 (polar ptt2 ang2 dist)
			)
			(cond
				((setq int (inters pt3 pt4 ptt3 ptt4))
					(setq pt3 int pt5 (inters pt5 pt6 ptt5 ptt6 nil))
				)
				((setq int (inters pt3 pt4 ptt5 ptt6))
					(setq pt3 int pt5 (inters pt5 pt6 ptt3 ptt4 nil))
				)
				((setq int (inters pt5 pt6 ptt5 ptt6))
					(setq pt5 int pt3 (inters pt3 pt4 ptt3 ptt4 nil))
				)
				((setq int (inters pt5 pt6 ptt3 ptt4))
					(setq pt5 int pt3 (inters pt3 pt4 ptt5 ptt6 nil))
				)
			)
		)
		) 

		(setq ss_pt2 (ssdel en (ssget "x" (list (cons -4 "<or") (cons 10 pt2) (cons 11 pt2) (cons -4 "or>") '(8 . "wall-base")))))
		(if (> (sslength ss_pt2) 0)
		(progn
			(setq 
				entt (entget (ssname ss_pt2 0))
				ptt1 (cdr (assoc 10 entt))
				ptt2 (cdr (assoc 11 entt))
				ang(angle ptt1 ptt2)
				ang1 (+ ang (/ pi 2))
				ang2 (- ang (/ pi 2))
				ptt3 (polar ptt1 ang1 dist)
				ptt4 (polar ptt2 ang1 dist)
				ptt5 (polar ptt1 ang2 dist)
				ptt6 (polar ptt2 ang2 dist)
			)
			(cond
				((setq int (inters pt3 pt4 ptt3 ptt4))
					(setq pt4 int pt6 (inters pt5 pt6 ptt5 ptt6 nil))
				)
				((setq int (inters pt3 pt4 ptt5 ptt6))
					(setq pt4 int pt6 (inters pt5 pt6 ptt3 ptt4 nil))
				)
				((setq int (inters pt5 pt6 ptt5 ptt6))
					(setq pt6 int pt4 (inters pt3 pt4 ptt3 ptt4 nil))
				)
				((setq int (inters pt5 pt6 ptt3 ptt4))
					(setq pt6 int pt4 (inters pt3 pt4 ptt5 ptt6 nil))
				)
			)
		)
		)

		(entmake (list '(0 . "line") (cons 8 FLLT_WALL) (cons 10 pt3) (cons 11 pt4) wl_xdt))
		(setq ss_tmp (ssget "f" (list pt3 pt4) (list (cons 8 FLLT_WALL))))
		(setvar "cmdecho" 0)
		(cmd "_.select" ss_xc ss_tmp "")
		(setq ss_xc (ssget "p"))
		(setq ss_xc (ssadd (entlast) ss_xc))
		(entmake (list '(0 . "line") (cons 8 FLLT_WALL) (cons 10 pt5) (cons 11 pt6) wl_xdt))
		(setq ss_tmp (ssget "f" (list pt5 pt6) (list (cons 8 FLLT_WALL))))
		(cmd "_.select" ss_xc ss_tmp "")
		(setq ss_xc (ssget "p"))
		(setq ss_xc (ssadd (entlast) ss_xc))
		(setq idx (1+ idx))
	)
	(setq wall_wid dist)
	(if ss_xc (l2w_xc ss_xc))
)

(defun xc_cnt ()
	(setq amnt (1+ amnt))
	(if num
		(progn
			(repeat num (write-char 8))
			(princ amnt)
			(setq num (strlen (rtos amnt 2 0)))
		)
		(progn
			(princ amnt)
			(setq num (strlen (rtos amnt 2 0)))
		)
	)
) 

(defun l2w_xc (ss_xc / amt en_lst int m ent1 ent2 en rpt pt1 pt2 pt1-1 pt1-2 pt2-1 pt2-2 amnt num l ent)
	(setq 
		amt (sslength ss_xc)
		idx 0
		en_lst (list)
	)
	(repeat amt
		(setq 
			en (ssname ss_xc idx)
			en_lst (append en_lst (list en))
			idx (1+ idx)
		)
		(redraw en 2)
	)
	(setq idx 0 amnt 0)
	(princ "\nWall-line: ")
	(setvar "ucsicon" 0)
	(repeat amt
		(setq 
			ent1 (entget (nth idx en_lst) '("FISH_WALL"))
			pt1-1 (cdr (assoc 10 ent1))
			pt1-2 (cdr (assoc 11 ent1))
			m (1+ idx)
		)
		(if (= m amt) (setq m 0))
		(cmd "_.ucs" "e" (cdr (assoc -1 ent1)))
		(setq int_lst (list (car (trans pt1-1 0 1))))
		(repeat (1- amt)
			(setq 
				ent2 (entget (nth m en_lst))
				pt2-1 (cdr (assoc 10 ent2))
				pt2-2 (cdr (assoc 11 ent2))
				int (inters pt1-1 pt1-2 pt2-1 pt2-2)
			)
			(if (member int (list pt1-1 pt1-2)) (setq int nil))
			(if int (setq int_lst (append int_lst (list (car (trans int 0 1))))))
			(setq m (1+ m))
			(if (= m amt) (setq m 0))
		)
		(setq int_lst (append int_lst (list (car (trans pt1-2 0 1)))))
		(setq int_lst (vl-sort int_lst '<))
		(if (< (- (cadr int_lst) (car int_lst)) (* 3 wall_wid))
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
			(xc_cnt)
			(setq l (+ l 2))
		)
		(setq idx (1+ idx))
	)
	(cmd "_.erase" ss_xc "")
	(redraw)
) 

(defun c:l2w (/ GorB grdmd ucsf dist ss)

	(princ "\nFishLISP. Oct 7 1997")
	(princ "\nConvert Lines to Wall-lines. Start as C:L2W")

	(setq grdmd (getvar "gridmode"))
	(setq ucsf (getvar "ucsfollow"))
	(fl_undo_begin)
	(setvar "cmdecho" 0)
	(setvar "gridmode" 0)
	(setvar "ucsfollow" 0)
	(cssave)
	(if wall_wid nil (setq wall_wid 100.0))
	(princ "\nWidth of the wall<")
	(princ (* 2 wall_wid))
	(setq dist (getint ">: "))
	(if dist
		(setq dist (/ dist 2.0))
		(setq dist wall_wid)
	)

	(princ "\nSelect Lines to convert: ")
	(setq ss (ssget '((0 . "line") (-4 . "<not") (8 . "wall*") (-4 . "not>"))))
	(setvar "highlight" 0)
	(if ss (l2w_ol ss))

	(csrestore)
	(setvar "ucsicon" 1)
	(setvar "gridmode" grdmd)
	(setvar "ucsfollow" ucsf)
	(fl_undo_end)
	(setvar "cmdecho" 1)
	(setvar "highlight" 1)

	(princ)
)

(setvar "cmdecho" 1)
(princ "loaded. Start as C:L2W ")
(princ)