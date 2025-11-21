; This LISP application is a good sample of
; function "subst" "cons" and "entmake"
; 
; This function is called by TK.LSP if the linetypscale changed while
; inserting tileblock
;
;	2025-11-21	optimization

(defun sctx (sc_ ss_ / idx entn ent hi_old hi_new ent_nw)
	(setq idx 0)
	(while (< idx (sslength ss_))
		(setq 
			entn (ssname ss_ idx)
			ent (entget entn)
			hi_old (cdr (assoc 40 ent))
			hi_new (* hi_old sc_)
		)
		(entmod (subst (cons 40 hi_new) (assoc 40 ent) ent))
		(setq idx (1+ idx))
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
