(defun c:fixt (/ ss idx txtent newstr k) 
	(setq 
		fx_pre (if fx_pre fx_pre "")
		fx_pos (if fx_pos fx_pos "")
	)
	(princ (strcat "\nPrefix: '" fx_pre "'\tPostfix: '" fx_pos "'"))
	(initget "Option Select")
	(setq 
		k (getkword "\n[Option]<Select text object>: ")
		k (if k k "Select")
	)
	(if (= k "Option") 
		(setq 
			fx_pre (getstring 1 "\nPrefix: ")
			fx_pos (getstring 1 "\nPostfix: ")
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
				newstr (strcat fx_pre (cdr (assoc 1 txtent)) fx_pos)
			)
			(entmod (subst (cons 1 newstr) (assoc 1 txtent) txtent))
			(setq idx (1+ idx))
		)
	)
	(princ)
)

;* C:mrgtxt	* Merge strings to one
;
(defun c:mrgt (/ txtmod mrgtsetup k k1 k2 txt0 ent0 str0 txt1 str1) 
	(setq olderr *error*)
	(defun *error* (s) 
		(setvar "cmdecho" 0)
		(fl_undo_end)
		(setq *error* olderr olderrnil)
		(princ)
	)
	(setvar "cmdecho" 0)
	(fl_undo_begin)

	(setq mrgtxtkeep (if mrgtxtkeep mrgtxtkeep "No"))
	(setq HorT (if HorT HorT "Tail"))
	(defun txtmod () 
		(setq str0 (strcat str1 str2))
		(entmod (subst (cons 1 str0) (assoc 1 ent0) ent0))
	)

	(defun mrgtsetup () 
		(princ 
			(strcat "\nOption: Keep additional text: <" mrgtxtkeep ">\tAdd to: <" HorT ">")
		)
		(initget "Option")
		(setq k (getkword "\nOption/<origenal text>: "))
		(if (= k "Option") 
			(progn 
				(princ "\nKeep additional text: <")
				(princ mrgtxtkeep)
				(princ "> ")
				(initget "Yes No")
				(setq k1 (getkword))
				(setq mrgtxtkeep (if k1 k1 mrgtxtkeep))
				(princ "\nAdd to ")
				(if (= HorT "Head") 
					(princ "<Head>/Tail: ")
					(princ "Head/<Tail>: ")
				)
				(initget "Head Tail")
				(setq k2 (getkword))
				(setq HorT (if k2 k2 HorT))
				(mrgtsetup)
			)
			(setq txt0 (entsel))
		)
	)

	(mrgtsetup)

	(if txt0 
		(progn 
			(setq 
				ent0 (entget (car txt0))
				str0 (cdr (assoc 1 ent0))
			)
			(while (setq txt1 (entsel "\nAdditional text, or RETURN to exit:")) 
				(setq str1 (cdr (assoc 1 (entget (car txt1)))))
				(if (eq HorT "Head") 
					(setq str2 str0)
					(setq
						str2 str1
						str1 str0
					)
				)
				(if (= mrgtxtkeep "No") 
					(entdel (car txt1))
				)
				(txtmod)
			)
		)
	)

	(fl_undo_end)
	(setvar "cmdecho" 1)
	(princ)
)

;
;* C:str+	* Pick numbers to add together and write it
;
(defun c:str+ (/ cl ss count n_plus th pt num) 
	(setvar "cmdecho" 0)
	(if (tblsearch "layer" FLLT_DIM0) 
		nil
		(cmd "layer" "n" FLLT_DIM0 "c" "g" FLLT_DIM0 "")
	)
	(setq
		ss (ssget '((0 . "text")))
		count0
		n_plus 0
		th (* 3 (getvar "ltscale"))
		pt (getpoint "\nPick a point to write value, or RETURN to ignore:")
	)
	(repeat (sslength ss) 
		(setq 
			num (read (cdr (assoc 1 (entget (ssname ss count)))))
			n_plus (+ n_plus num)
			count(1+ count)
		)
	)
	(setq n_plus (rtos n_plus 2 1))
	(if pt 
		(progn 
			(setvar "clayer" "t")
			(cmd "text" pt th "0" n_plus)
		)
		(princ (strcat "\nThe total is " n_plus))
	)
	(if cl (setvar "clayer" cl))
	(setvar "cmdecho" 1)
	(princ)
)

(princ "loaded.\nFishLISP. Text entity utilities. Start as C:FIXT C:MRGT C:STR+ " )
(princ)

