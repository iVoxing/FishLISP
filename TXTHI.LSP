; This LISP application is a good sample of
; function "subst" "cons" and "entmake"
; 
; This function is called by TK.LSP if the linetypscale changed while
; inserting tileblock
;
(defun sctx (sc ss / count entn ent hi_old hi_new ent_nw)
	(setq count 0)
	(while (< count (sslength ss))
		(setq 
			entn (ssname ss count)
			ent (entget entn)
			hi_old (cdr (assoc 40 ent))
			hi_new (* hi_old sc)
		)
		(entmod (subst (cons 40 hi_new) (assoc 40 ent) ent))
		(setq count (1+ count))
	)
	(princ)
)

;* C:txthi 	* Scale Text seperately
(defun c:sctx (/ sc ss)
	(princ "\nFishLISP. 1996-05-08")
	(princ "\nScale Text seperately. Start as C:SCTX")
	(while (not sc)
		(setq sc (getreal "\nScale Value:"))
	)
	(prompt "\nSelect Text to edit:")
	(setq ss (ssget '((0 . "TEXT"))))
	(sctx sc ss)
	(princ)
)
