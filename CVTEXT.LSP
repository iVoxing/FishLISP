; Text ‰≥ˆ ‰»Î

(defun c:txtout (/ ss amt idx f ent0 ent txt newtxt pz cha)
	(princ "\nFishLISP, 6-4-1998. Ver 1.0.")
	(setq ss (ssget '((0 . "text"))))
	(if ss
		(setq amt (sslength ss) idx 0 f (open (strcat (getvar "dwgprefix") (getvar "dwgname") ".lsp") "w"))
		(setq amt 0)
	)

	(repeat amt
		(setq 
			ent0 (entget (ssname ss idx))
			txt (cdr (assoc 1 ent0))
		)
		(if (wcmatch txt "*\"*")
			(progn
				(setq 
					newtxt "" 
					pz 1
				)
				(repeat (strlen txt)
					(setq 
						cha (substr txt pz 1)
						newtxt (strcat newtxt (if (= cha "\"") "\\\"" cha))
						pz (1+ pz)
					)
				)
			)
			(setq newtxt txt)
		)
		(setq 
			chk newtxt 
			chk2 txt 
			ent (list 
					(assoc 0 ent0)
					(cons 1 newtxt)
					(assoc 7 ent0)
					(assoc 8 ent0)
					(assoc 10 ent0)
					(assoc 11 ent0)
					(assoc 40 ent0)
					(assoc 41 ent0)
					(assoc 50 ent0)
					(assoc 51 ent0)
				)
		)
		(princ "(entmake '" f)
		(prin1 ent f)
		(princ ")\n" f)
		(setq idx (1+ idx))
	)
	(close f)
	(alert (strcat 
		"\n" (rtos amt 2 0) " Text object(s) exported."
		"\nTo import the text:"
		"\n 1. Open the same drawing in R14;"
		"\n 2. Load CVTEXT.LSP again;"
		"\n 3. Enter C:TXTIN to import the text."
	))
	(setq *error* olderr)
	(princ)
)

(defun c:txtin (/ f)
	(if (setq f (findfile (strcat (getvar "dwgprefix") (getvar "dwgname") ".lsp")))
		(load f)
		(alert "No exported text found! ")
	)
	(princ)
)

(princ "loaded. Start as C:TXTOUT or C:TXTIN. ")
(princ)