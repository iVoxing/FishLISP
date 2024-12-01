

(defun 2of_option (/ dcl_id layer_list next_step)
	(setq next_step 2)
	(while (>= next_step 2)
		(if	(and (not dcl_id) (minusp (setq dcl_id (load_dialog "DD2OF"))))
			(exit)
		)
		(if	(not (new_dialog "DD2OF" dcl_id))
			(exit)
		)

; ========= initialize dialogue ===========

		(if	of_lay
			(progn
				(set_tile "chlay" "1")
				(mode_tile "layerlist" 0)
				(mk_list)
			)
			(progn
				(set_tile "chlay" "0")
				(mode_tile "layerlist" 1)
			)
		)
		(if	of_e
			(set_tile "delold" "1")
			(set_tile "delold" "0")
		)
		(if	of_dist
			(set_tile "ofdist" (rtos of_dist))
		)

; ============== actions ===================

		(action_tile "chlay" "(chlay_act)")
		(action_tile "accept" "(accept_option)")
		(action_tile "no" "(done_dialog 0)")
		(action_tile "pickdis" "(done_dialog 4)")
		(setq next_step (start_dialog))
		(if	(= next_step 4)
			(setq of_dist (getdist "\nOffset distanc: "))
		)
	)
)

(defun chlay_act ()
	(if (= (get_tile "chlay") "0")
		(progn
			(start_list "layerlist")
			(end_list)
			(mode_tile "layerlist" 1)
		)
		(progn
			(mode_tile "layerlist" 0)
			(mk_list)
		)
	)
)

(defun accept_option ()
	(if (= (get_tile "chlay") "0")
		(setq of_lay nil)
		(setq of_lay (nth (atoi (get_tile "layerlist")) layer_list))
	)
	(if (= (get_tile "delold") "1")
		(setq of_e 1)
		(setq of_e nil)
	)
	(setq dist (distof (get_tile "ofdist")))
	(done_dialog 1)
)

(defun mk_list (/ layer_name)
	(setq layer_list (list))
	(setq layer_name (cdr (assoc 2 (tblnext "layer" t))))
	(while layer_name
		(if	(= layer_name of_lay)
			nil
			(setq layer_list (append layer_list (list layer_name)))
		)
		(setq layer_name (cdr (assoc 2 (tblnext "layer"))))
	)
	(setq layer_list (acad_strlsort layer_list))
	(if of_lay
		(setq layer_list (append (list of_lay) layer_list))
	)
	(start_list "layerlist")
	(mapcar 'add_list layer_list)
	(end_list)
)

;|Ë«ÏòÆ«ÒÆ SNSJ 2004.3.8
(defun ptlay (x y)
	(if y
		(vla-put-layer
			(car (vlax-safearray->list (vlax-variant-value x)))
			y
		)
	)
)
(defun c:dft (/ obj ss)
	(vl-load-com)
	(princ "\nby SNSJ and Little Fish. Offset object(s) to double side.")
	(if of_dist
		(progn
			(if of_lay
				(princ (strcat "\nTarget layer: " (strcase of_lay) ", "))
				(princ "\nKeep layer, ")
			)
			(if of_e
				(princ "Delete old line(s), ")
				(princ "Keep old line(s), ")
			)
			(princ "Distance: ")
			(if of_dist
				(princ of_dist)
				(princ "0.0")
			)
			(initget "Option")
			(setq kwd (getkword "\n[Option] <select>: "))
			(if (= kwd "Option")
				(2of_option)
			)
		)
		(2of_option)
	)
	(setq ss (ssget '((0 . "Arc,Circle,Ellipse,*Line"))))
	(if ss
		(progn
			(vlax-for obj 
				(vla-get-activeselectionset
					(vla-get-activedocument (vlax-get-acad-object))
				)
				(ptlay (vla-offset obj dist) of_lay)
				(ptlay (vla-offset obj (* dist -1)) of_lay)
			)
			(setq of_dist dist)
			(if of_e
				(progn
					(setvar "cmdecho" 0)
					(cmd "erase" ss "")
					(setvar "cmdecho" 1)
				)
			)
		)
	)
	(princ)
)

(princ "\n Start as C:DFT")
(princ)