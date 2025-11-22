; FISHLISP
; C:BKC
; Change layer(s), color(s) of entities within a block.
; History:
; 2003-08-15 v1.0 original version.

(defun c:bkc (/ olderr bkc_alias bk en ent cnt) 
	(setq olderr *error*)
	(defun *error* (s_) 
		(setq *error* olderr)
		(*error* s_)
		(cmd "regen")
		(princ)
	)
	(setq bkc_layer (if bkc_layer bkc_layer "0"))
	(setq bkc_color (if bkc_color bkc_color 0))
	(setq bkc_color_list (list 
			"ByBlock" 
			"Red"
			"Yellow"
			"Green"
			"Cyan"
			"Blue"
			"Magenta"
			"White"
	))
	(setq bkc_alias (cond 
		((< bkc_color 8) (nth bkc_color bkc_color_list))
		((= bkc_color 256) "ByLayer")
		(t (rtos bkc_color 2 0))
	))
	(princ "\nFISHLISP: BKC v1.0. Little Fish Studio. 2003-08-15")
	(princ (strcat "\nTarget Layer: " bkc_layer ", target Color: " bkc_alias))
	(while (setq bk (getbk)) 
		(setq en (cdr (assoc -2 (tblsearch "block" bk))))
		(setq cnt 0)
		(while en 
			(setq ent (entget en))
			(setq ent (subst (cons 8 bkc_layer) (assoc 8 ent) ent))
			(if (assoc 62 ent) 
				(setq ent (subst (cons 62 bkc_color) (assoc 62 ent) ent))
				(if (= bkc_color 256) 
					nil
					(setq ent (append ent (list (cons 62 bkc_color))))
				)
			)
			(entmod ent)
			(setq cnt (1+ cnt))
			(setq en (entnext en))
		)
		(princ (strcat "\n" (rtos cnt 2 0) " entitie(s) in Block " bk " changed. Regen required. "))
	)
	(princ "\n")
	(cmd "regen")
	(setq *error* olderr)
	(princ)
)

(defun getbk (/ bk_en bl bc bk_name) 
	(initget "laYer Color")
	(setq bk_en (entsel "\n[laYer/Color] Select INSERT to edit: "))
	(cond 
		((= bk_en "laYer")
			(princ (strcat "\nTarget Layer: <" bkc_layer "> "))
			(setq bl (getstring t))
			(setq bl (cond 
				((= bl "") bkc_layer)
				((not (tblsearch "layer" bl))
					(princ "\nLayer not found. ")
					bkc_layer
				)
				(t bl)
			))
			(setq bkc_layer bl)
			(getbk)
		)
		((= bk_en "Color")
			(initget "byBlock byLayer Red Yellow Green Cyan BLue Magenta White")
			(setq bc (getint (strcat "\nTarget Color: <" bkc_alias "> ")))
			(setq bcc (member bc bkc_color_list))
			(setq bc (cond 
				((not bc) bkc_color)
				(bcc
					(- 8 (length bcc))
				)
				((= bc "byLayer") 256)
				(t
					(if (>= 256 bc 0) 
						bc
						(progn 
							(princ "\nInvalid color index ignored! ")
							bkc_color
						)
					)
				)
			)
			)
			(setq bkc_color bc)
			(getbk)
		)
		((listp bk_en)
			(setq bk_ent (entget (car bk_en)))
			(setq bk_name (if (= "INSERT" (cdr (assoc 0 bk_ent))) (cdr (assoc 2 bk_ent))))
			bk_name
		)
		(t nil)
	)
)

(princ "loaded. Start as C:BKC ")
(princ)