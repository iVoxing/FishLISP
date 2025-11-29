; Modification:
;	1. 12/24/97 Fillet function added, but doesnot work properly.
;
;;; test


(defun wall_query (/ es ent xdt)
	(while (setq es (entsel "\nPick wall: "))
		(setq ent (entget (car es) '("FL_WALL")))
		(if (setq xdt (cadr (assoc -3 ent)))
			(princ (strcat 
				"\nWall ID: "
				(cdr (assoc 1000 (cdr xdt)))
				"\tType: "
				(cdr (nth 2 (cdr xdt)))
				"\tWidth: "
				(rtos (cdr (assoc 1041 (cdr xdt))) 2 0 )
			))
			(princ "\n1 was not a registered wall. ")
		)
	)
)

(defun get_wall (/ es ent wall_id ss ss0)
	(setq es (entsel "\nPick wall: "))
	(if es
		(setq 
			ent (entget (car es) '("FL_WALL"))
			wall_id (cdr (cadadr (assoc -3 ent)))
		)
		(setq ss nil)
	)
	(if wall_id
		(setq 
			ss0 (ssget "x" '((-3 ("FL_WALL"))))
			rpt (if ss0 (sslength ss0) 0)
			ss (ssadd) 
			idx 0
		)
		(alert "1 was not a registered wall. ")
	)
	(repeat rpt
		(setq en (ssname ss0 idx))
		(if (= (cdr (cadadr (assoc -3 (entget en '("FL_WALL"))))) wall_id)
			(setq ss (ssadd en ss))
		)
		(setq idx (1+ idx))
	)
	ss
)
 
(defun wall_highlight (ss_ / idx)
	(setq idx 0)
	(repeat (sslength ss_)
		(redraw (ssname ss_ idx) 3)
		(setq idx (1+ idx))
	)
)

(defun wall_del_single (/ ss)
	(while (setq ss (get_wall))
		(cmd "_.erase" ss "")
	)
	(call_updt)
)

(defun wall_move (/ ss)
	(setq ss (ssadd))
	(while (setq ss0 (get_wall))
		(cmd "_.select" ss ss0 "")
		(setq ss (ssget "p"))
		(wall_highlight ss)
	)
	(if ss
		(progn
			(command "_.move" ss "")
			(princ "\nBase point or displacement: ")
			(command pause)
			(princ "\nSecond point of displacement: ")
			(command pause)
		)
	)
	(call_updt)
)

(defun call_updt (/ kwd)
	(initget "Yes No")
	(setq kwd (getkword "\nUpdate Walls? <Yes>"))
	(setq kwd (if kwd kwd "Yes"))
	(if (= kwd "Yes") (wall_update))
)

(defun wall_del_ss (/ ss)
	(setq ss (ssadd))
	(while (setq ss0 (get_wall))
		(cmd "_.select" ss ss0 "")
		(setq ss (ssget "p"))
		(wall_highlight ss)
	)
	(if ss (cmd "_.erase" ss ""))
	(call_updt)
)

(defun wall_fillet (/ es1 ent1 wall_id1 ss1 es2 ent2 wall_id2 ss2 dist)
	(setq es1 (entsel "\nPick wall: "))
	(if es1
		(progn
			(setq ent1 (entget (car es1) '("FL_WALL")))
			(if (setq wall_id1 (caadr (assoc -3 ent1)))
				(progn
					(setq ss1 (ssget "x" (list (list -3 (list wall_id1)))))
					(wall_highlight ss1)
					(setq 
						es1 (handent (substr wall_id1 11))
						ss1 (ssdel es1 ss1)
					)
				)
				(progn
					(alert "1 was not a registered wall. ")
					(setq es1 nil ss1 nil)
				)
			)
		)
	)
	(if es1 (setq es2 (entsel "\nPick Wall: ")))
	(if es2
		(progn
			(setq ent2 (entget (car es2) '("FL_WALL")))
			(if (setq wall_id2 (caadr (assoc -3 ent2)))
				(progn
					(setq ss2 (ssget "x" (list (list -3 (list wall_id2)))))
					(wall_highlight ss2)
					(setq 
						es2 (handent (substr wall_id2 11))
						ss2 (ssdel es2 ss2)
					)
				)
				(progn
					(alert "1 was not a registered wall. ")
					(setq es2 nil ss2 nil)
				)
			)
		)
	)
	(if es2
		(progn
			(cmd "_fillet" es1 es2)
			(cmd "_erase" ss1 ss2 "")
			(cmd "_select" es1 es2 "")
			(setq wb_ss (ssget "P"))
			(setq dist (/ (cdr (assoc 1041 (cdadr (assoc -3 (entget es1 '("FL_WALL")))))) 2.0))
			(l2w_ol wb_ss)
		)
	)
)

(defun wall_update (/ dist)
	(setq ss (ssadd))
	(while (setq ss0 (get_wall))
		(cmd "_.select" ss ss0 "")
		(setq ss (ssget "p"))
		(wall_highlight ss)
	)
	(setvar "highlight" 0)
	(setq wb_ss (ssget "p" '((8 . "wall-base"))))
	(cmd "_.select" ss "")
	(setq wl_ss (ssget "p" (list (cons 8 FLLT_WALL))))
	(if wl_ss (cmd "_.erase" wl_ss ""))
	(if wb_ss (setq dist (/ (cdr (assoc 1041 (cdadr (assoc -3 (entget (ssname wb_ss 0) '("FL_WALL")))))) 2.0)))
	(if wb_ss (l2w_ol wb_ss))
	(setvar "highlight" 1)
)

(defun c:mdw (/ k)
	(setq mdw_key (if mdw_key mdw_key "Query"))
	(princ "\nModify or query walls.")
	(initget "Erase Move Update Query Width Fillet")
	(setq k (getkword (strcat "\nErase/Move/Fillet/Width/Update/Query/<" mdw_key ">: ")))
	(setq k (if k k mdw_key))
	(setq mdw_key k)
	(setvar "cmdecho" 0)
	(cond
		((= k "Erase")	(wall_del_single))
		((= k "Move")	(wall_move))
		((= k "Fillet")	(wall_fillet))
		((= k "Update")	(wall_update))
		((= k "Width") 	(alert "Not ready"))
		((= k "Query") 	(wall_query))
		(t)
	)
	(setvar "cmdecho" 1)
	(princ)
)

(princ "loaded. Start as C:MDW ")
(princ)
