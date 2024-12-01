; 2001/13/98	Bug: Not work in UCS.
; 2008/28/99	v2.6	更灵活的Symbol选择.
; 2001/24/14	v3.0	增加圖紙單位為米時的處理

(setvar "cmdecho" 0)
(if (tblsearch "layer" FLLT_DIM0)
	nil
	(cmd "layer" "n" FLLT_DIM0 "c" "g" FLLT_DIM0 "")
)
(setvar "cmdecho" 1)

(defun c:elin (/ olderr os cl el_sc aq unit_scale)
	(setq olderr *error*)
	(defun *error* (s)
		(if os (setvar "osmode" os))
		(if cl (setvar "clayer" cl))
		(if aq (setvar "attreq" aq))
		(setvar "cmdecho" 0)
		(undoend)
		(setvar "cmdecho" 1)
		(setq *error* olderr olderr nil os nil cl nil aq nil)
		(princ)
	)
	(prompt "\nFishLISP. 12/13/1995. v2.6.")
	(setq
		os (getvar "osmode")
		cl (getvar "clayer")
		aq (getvar "attreq")
	)
	(setq unit_scale (if (= (getvar "insunits") 6) 1.0 1000.0))
	(setvar "cmdecho" 0)
	(setvar "dimzin" 1)
	(setvar "attreq" 1)
	(cmd "_undo" "g")
	(setvar "clayer" FLLT_DIM0)
	(if el_h nil (el_ini))
	(if el_blk nil (setq el_blk "UR"))
	(setq el_sc (getvar "dimscale"))
	(el_main)
	(setvar "osmode" os)
	(setvar "clayer" cl)
	(setvar "cmdecho" 0)
	(setvar "attreq" aq)
	(cmd "_undo" "end")
	(setvar "cmdecho" 1)
	(setq *error* olderr)
	(princ)
)

(defun el_main ()
	(prompt "\n?/UNload/Copy/Move/Array/symbol:[")
	(princ (strcase el_blk))
	(initget "? UNload Copy Move Array UL UR DL DR Plan Value Base")
	(setq pt1 (getpoint "]/Value/Plan mode/Base point/<insert point>: "))
	(cond
		((listp pt1) (el_ins))
		((= pt1 "Copy")	(el_cop))
		((= pt1 "Move")	(el_mov))
		((= pt1 "Array")	(el_arr))
		((member pt1 (list "UL" "UR" "DL" "DR"))
			(setq el_blk pt1)
			(el_main)
		)
		((= pt1 "Plan")
			(defun c:elin ()
				(el_pla)
				(princ)
			)
			(c:elin)
		)
		((= pt1 "Value") (el_val))
		((= pt1 "?") (el_hlp))
		((= pt1 "Base")	(el_bas))
		((= pt1 "UNload") (elunld))
		(t)
	)
)

(defun el_ins (/ el_y1 cl loop)
	(while pt1
		(setq el_y1 (rtos (/ (- (cadr pt1) el_h) unit_scale) 2 3))
		(ins_el el_y1)
		(setq loop t)
		(while loop
			(initget "UL UR DL DR")
			(princ (strcat "\nSymbol:[" el_blk "]/"))
			(setq pt1 (getpoint "<insert point>: "))
			(if (listp pt1)
				(setq loop nil)
				(setq el_blk pt1)
			)
		)
	)
)

(defun ins_el (attr / tmpval)
	(princ (strcat "\nElevation: <" attr))
	(setq tmpval (getstring ">: "))
	(if (= tmpval "") nil (setq attr tmpval))
	(if (= attr "0.000") (setq attr "%%P0.000"))
	(cmd "_undo" "end")
	(cmd "_undo" "g")
	(setvar "osmode" 0)
	(cmd "_insert" (strcat "EL" el_blk) pt1 el_sc el_sc 0 attr)
	(setvar "osmode" os)
)

(defun el_cop (/ en ent a0 y0 pt0 pt1)
	(setq en (car (entsel "\nPick an ELevation-Block to edit: ")))
	(if en
		(if
			(and
				(= (cdr (assoc 0 (setq ent (entget en)))) "INSERT")
				(= (substr (cdr (assoc 2 (entget en))) 1 2) "EL")	
			)
			(progn
				(setq
					pt0	(cdr (assoc 10 ent))
					y0	(cadr (trans pt0 0 1))
					a0	(distof (cdr (assoc 1 (entget (entnext en)))))
				)
				(if a0 nil (setq a0 0.0))
				(while (setq pt1 (getpoint (trans pt0 0 1) "\nInsert point: "))
					(ins_el (rtos (+ a0 (/ (- (cadr pt1) y0) units_scale)) 2 3))
				)
			)
			(princ "\n1 was not ELevation-Block. ")
		);if and
	); if en
)

(defun el_mov (/ ss rpt pt1 pt2 ang dis val count en_el ent_el pt_el en_att ent_att pt_att attr)
	(prompt "\nSelect ELevation-Block(s) to edit: ")
	(setq ss (ssget '((0 . "insert") (2 . "el??"))))
	(if ss
		(progn
			(setq
				rpt	(sslength ss)
				pt1	(getpoint "\nBase point or displacement: ")
				pt2	(getpoint pt1 "Second point of displacement: ")
				ang	(angle (trans pt1 1 0) (trans pt2 1 0))
				a	(/ (* ang 180) pi)	
				dis	(distance pt1 pt2)
				d	dis
				val	(/ (- (cadr pt2) (cadr pt1)) unit_scale)
				v	val
				idx	0
			)
			(repeat rpt
				(setq
					en_el	(ssname ss idx)
					ent_el	(entget en_el)
					en_att	(entnext en_el)
					ent_att	(entget en_att)
					pt_el	(polar (cdr (assoc 10 ent_el)) ang dis)
					pt_att	(polar (cdr (assoc 10 ent_att)) ang dis)
					pt_att2	(polar (cdr (assoc 11 ent_att)) ang dis)
					attr0	(cdr (assoc 1 ent_att))
					attr	(rtos (+ (if (= attr0 "%%P0.000") 0.0 (distof attr0)) val) 2 3)
				)
				(if (= attr "0.000") (setq attr "%%P0.000"))
				(entmod (subst (cons 10 pt_el) (assoc 10 ent_el) ent_el))
				(setq
					ent_att (subst (cons 10 pt_att) (assoc 10 ent_att) ent_att)
					ent_att (subst (cons 11 pt_att2) (assoc 11 ent_att) ent_att)
				)
				(entmod (subst (cons 1 attr) (assoc 1 ent_att) ent_att))
				(entupd en_el)
				(setq idx (1+ idx))
			);repeat
		);progn
	);if ss
)

(defun el_arr (/ ss os cl rpt idx row dis en_el ent_el pt_el0 bkname attr0 times pt_el1 attr1)
	(prompt "\nSelect ELevation-Block(s) to edit: ")
	(setq ss (ssget '((0 . "insert") (2 . "el??"))))
	(if ss
		(progn
			(setq os (getvar "osmode"))
			(setvar "osmode" 0)
			(setvar "cmdecho" 0)
			(setvar "clayer" FLLT_DIM0)
			(setq
				rpt (sslength ss) g_rpt rpt
				idx 0
				row (getint "\nNumber of rows (---): ")
				dis (getdist "\nDistance between rows (---): ")
			)
			(repeat rpt
				(setq
					en_el	(ssname ss idx)
					ent_el	(entget en_el)
					pt_el0	(cdr (assoc 10 ent_el))
					bkname	(cdr (assoc 2 ent_el))
					attr0	(cdr (assoc 1 (entget (entnext en_el))))
					times	1
				)
				(while (< times row)
					(setq
						pt_el1	(subst (+ (cadr pt_el0) (* dis times)) (cadr pt_el0) pt_el0)
						attr1	(rtos (+ (distof attr0) (/ dis times unit_scale)) 2 3)
					)
					(if (= attr1 "0.000") (setq attr1 "%%P0.000"))
					(setvar "osmode" 0)
					(cmd "_insert" bkname pt_el1 el_sc el_sc 0 attr1)
					(setvar "osmode" os)
					(setq times (1+ times))
				);while
				(setq idx (1+ idx))
			);repeat
			(setvar "osmode" os)
			(setvar "cmdecho" 1)
			(setvar "clayer" cl)
		);progn
	);if
)

(defun el_pla (/ pt1)
	(setq el_cop nil el_mov nil el_arr nil el_h nil el_main nil el_ins nil ins_el nil)
	(princ (strcat "\nElevation mode/Style," el_blk))
	(initget "Elevation Style Insert")
	(setq pt1 (getpoint "/<Insert point>"))
	(cond
		((listp pt1) (pla_ins))
		((= pt1 "Elevation")
			(load "elins2")
			(c:elin)
		)
		((= pt1 "Style") (el_sty))
		((= pt1 "Insert") (pla_ins))
		(t)
	)
)

(defun pla_ins (/ os)
	(setq os (getvar "osmode"))
	(setvar "osmode" 0)
	(setvar "cmdecho" 0)
	(if (= (type pt1) 'STR) (setq pt1 (getpoint "\nInsert point:")))
	(while pt1
		(setq str (getstring "\nLevel of this point: "))
		(setvar "osmode" 0)
		(cmd "_insert" (strcat "EL" el_blk) pt1 el_sc el_sc 0 str)
		(setvar "osmode" os)
		(setq pt1 (getpoint "\nInsert point: "))
	)
	(setvar "osmode" os)
	(setvar "cmdecho" 1)
)

(defun el_val ()
	(alert "Module not defuned yet!")
)

(defun el_hlp ()
	(alert "Module not defuned yet!\nbut you can turn to Litte Fish for help.")
)

(defun el_bas (/ hx)
	(setq el_bpt (getpoint "\nBase point: "))
	(setq hx (getreal "\nLevel of this point<0.000>: "))
	(if (= hx nil) (setq hx 0.0))
	(setq el_h (- (cadr el_bpt) (* hx unit_scale)))
	(el_main)
)

(defun el_ini (/ pt0 hx)
	(initget "Plan")
	(setq pt0 (getpoint "\nPlan mode/<base point>: "))
	(cond
		((listp pt1)
			(setq hx (getreal "\nLevel of this point<0.000>: "))
			(if (= hx nil) (setq hx 0.0))
			(setq el_h (- (cadr pt0) (* hx unit_scale)))
			(setq el_bpt pt0)
		)
		((= pt0 "Plan")
			(defun c:elin ()
				(el_pla)
				(princ)
			)
			(c:elin)
		)
	)
	(if (= (tblsearch "layer" FLLT_DIM0) nil)
		(cmd "layer" "n" FLLT_DIM0 "c" "g" FLLT_DIM0 "")
	)
	(setvar "attmode" 1)
	(princ)
)

(defun elunld ()
	(mapcar
		'(lambda (x) (setq x nil))
		'(el_main el_ins ins_el el_cop el_mov el_arr el_pla pla_ins el_val el_hlp el_bas el_ini elunld el_h el_blk)
	)
	(defun c:elin () (load "elins2") (c:elin) (princ))
	(princ "\nAppliction unloaded. Type ELIN to reload and run.")
)

(defun el_und ()
	(cmd "_undo" "e")
	(cmd "_undo")
	(elins)
)

(princ)