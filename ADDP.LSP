; 2025-11-19	rewrite

; add prefix or postfix to text en
(defun c:fixt (/ ss idx txtent newstr k) 
	(setq 
		FX_PRE (if FX_PRE FX_PRE "")
		FX_POST (if FX_POST FX_POST "")
	)
	(princ (strcat "\nPrefix: '" FX_PRE "'\tPostfix: '" FX_POST "'"))
	(initget "Option Select")
	(setq 
		k (getkword "\n[Option]<Select text object>: ")
		k (if k k "Select")
	)
	(if (= k "Option") 
		(setq 
			FX_PRE (getstring 1 "\nPrefix: ")
			FX_POST (getstring 1 "\nPostfix: ")
		)
	)
	(setq 
		ss (ssget '((0 . "text")))
		idx 0
	)
	(if ss 
		(repeat (sslength ss) 
			(setq
				txtent (entget (ssname ss idx))
				newstr (strcat FX_PRE (cdr (assoc 1 txtent)) FX_POST)
			)
			(entmod (subst (cons 1 newstr) (assoc 1 txtent) txtent))
			(setq idx (1+ idx))
		)
	)
	(princ)
)

(defun mrgtsetup (/ k k1 k2) 
	(princ (strcat "\nOption: Keep additional text: <" MRG_TXT_KEEP ">\tAdd to: <" HorT ">"))
	(initget "Option")
	(setq k (getkword "\nOption/<origenal text>: "))
	(if (= k "Option") 
		(progn 
			(initget "Yes No")
			(setq k1 (getkword (strcat "\nkeep additional text: <" MRG_TXT_KEEP "> ")))
			(setq MRG_TXT_KEEP (if k1 k1 MRG_TXT_KEEP))

			(setq fix_kw (if (= HorT "Head") "<Head>/Tail: " "Head/<Tail>: "))
			(initget "Head Tail")
			(setq k2 (getkword (strcat "\nAdd to " fix_kw)))
			(setq HorT (if k2 k2 HorT))
			(mrgtsetup)
		)
	)
)

; C:mrgtxt	Merge strings to one
(defun c:mrgt (/ txt0 ent0 str0 txt1 str1) 
	(setq olderr *error*)
	(defun *error* (s) 
		(setvar "cmdecho" 0)
		(fl_undo_end)
		(setq *error* olderr olderr nil)
		(princ)
	)
	(setvar "cmdecho" 0)
	(fl_undo_begin)

	(setq MRG_TXT_KEEP (if MRG_TXT_KEEP MRG_TXT_KEEP "No"))
	(setq HorT (if HorT HorT "Tail"))

	(mrgtsetup)

	(if (setq txt0 (entsel)) 
		(progn 
			(setq 
				ent0 (entget (car txt0))
				str0 (cdr (assoc 1 ent0))
			)
			(while (setq txt1 (entsel "\nAdditional text: <Exit>")) 
				(setq str1 (cdr (assoc 1 (entget (car txt1)))))
				(if (eq HorT "Head") 
					(setq str2 str0)
					(setq str2 str1 str1 str0)
				)
				(if (= MRG_TXT_KEEP "No") 
					(entdel (car txt1))
				)
				(entmod (subst (cons 1 (strcat str1 str2)) (assoc 1 ent0) ent0))
			)
		)
	)

	(fl_undo_end)
	(princ)
)

;* C:stradd	* Pick numbers to add together and write it
; 2025-11-19	rewrite, dtext height follow the first en
(defun c:stradd (/ ss idx n_plus hei lay pt num cla) 
	(setvar "cmdecho" 0)
	(if (tblsearch "layer" FLLT_DIM0) 
		nil
		(cmd "layer" "n" FLLT_DIM0 "c" "g" FLLT_DIM0 "")
	)
	(setq
		ss (ssget '((0 . "text")))
		idx 0
		n_plus 0
	)
	(repeat (sslength ss) 
		(setq 
			ent (entget (ssname ss idx))
			num (read (cdr (assoc 1 ent)))
			hei (if hei hei (cdr (assoc 40 ent)))
			lay (if lay lay (cdr (assoc 8 ent)))
			n_plus (+ n_plus num)
			idx (1+ idx)
		)
	)
	(setq n_plus (rtos n_plus 2 1))
	(setq pt (getpoint (strcat "\nSum: " n_plus ". Positon to write value: <ignore>")))
	(if pt
		(progn
			(setq cla (getvar "clayer"))
			(setvar "clayer" lay)
			(cmd "text" pt hei "0" n_plus)
			(setvar "clayer" cla)
		)
	)
	(setvar "cmdecho" 1)
	(princ)
)

(princ "loaded.\nFishLISP. Text entitie utilities. C:FIXT C:MRGT C:STRADD " )
(princ)

