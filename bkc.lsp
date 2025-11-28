; FISHLISP
; C:BKC
; Change layer(s), color(s) of entities within a block.
; History:
; 2025-11-26		optimization, test ok
; 2003-08-15 v1.0	original version.

(defun c:bkc (/ bkc_alias bk en ent amt)
	(defun *error* (s_)
		(setvar "cmdecho" 0)
		(cmd "_regen")
		(setvar "cmdecho" 1)
		(princ (strcat "\n** " s_ " **"))
		(setq *error* *pub_error*)
		(princ)
	)
	(setq BKC_LAYER (if BKC_LAYER BKC_LAYER "0"))
	(setq BKC_COLOR (if BKC_COLOR BKC_COLOR 0))
	(setq bkc_color_list
		(list 
			"ByBlock" 
			"Red"
			"Yellow"
			"Green"
			"Cyan"
			"Blue"
			"Magenta"
			"White"
		)
	)
	(setq bkc_alias
		(cond 
			((< BKC_COLOR 8) (nth BKC_COLOR bkc_color_list))
			((= BKC_COLOR 256) "ByLayer")
			(t (rtos BKC_COLOR 2 0))
		)
	)
	(princ (strcat "\nTarget Layer: " BKC_LAYER ", target Color: " bkc_alias))
	(while (setq bk (get_block))
		(setq en (cdr (assoc -2 (tblsearch "block" bk))))
		(setq amt 0)
		(while en 
			(setq ent (entget en))
			(setq ent (subst (cons 8 BKC_LAYER) (assoc 8 ent) ent))
			(if (assoc 62 ent)
				(setq ent (subst (cons 62 BKC_COLOR) (assoc 62 ent) ent))
				(if (= BKC_COLOR 256)
					nil
					(setq ent (append ent (list (cons 62 BKC_COLOR))))
				)
			)
			(entmod ent)
			(setq amt (1+ amt))
			(setq en (entnext en))
		)
		(princ (strcat "\n" (rtos amt 2 0) " entitie(s) in Block " bk " changed. Regen required. "))
	)
	(princ "\n")
	(cmd "_regen")
	(setq *error* *pub_error*)
	(princ)
)

(defun get_block (/ bk_en bl bc bk_name)
	(initget "laYer Color")
	(setq bk_en (entsel "\n[laYer/Color] Select INSERT to edit: "))
	(cond 
		((= bk_en "laYer")
			(princ (strcat "\nTarget Layer: <" BKC_LAYER "> "))
			(setq bl (getstring t))
			(setq bl (cond 
				((= bl "") BKC_LAYER)
				((not (tblsearch "layer" bl))
					(princ "\nLayer not found. ")
					BKC_LAYER
				)
				(t bl)
			))
			(setq BKC_LAYER bl)
			(get_block)
		)
		((= bk_en "Color")
			(initget "byBlock byLayer Red Yellow Green Cyan BLue Magenta White")
			(setq bc (getint (strcat "\nTarget Color: <" bkc_alias "> ")))
			(setq bcc (member bc bkc_color_list))
			(setq bc
				(cond 
					((not bc) BKC_COLOR)
					( bcc (- 8 (length bcc)))
					((= bc "byLayer") 256)
					((>= 256 bc 0) bc);; not tested
					(t 	
						(princ "\nInvalid color index ignored! ")
						BKC_COLOR
					)
				)
			)
			(setq BKC_COLOR bc)
			(get_block)
		)
		( bk_en
			(setq bk_ent (entget (car bk_en)))
			(setq bk_name (if (= "INSERT" (cdr (assoc 0 bk_ent))) (cdr (assoc 2 bk_ent))))
			bk_name
		)
		(t nil)
	)
)

(princ "loaded. Start as C:BKC ")
(princ)