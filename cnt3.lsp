(defun c:cnt (/ typ en ent lay ss strlist sslen idx str stramnt amt)
	(setvar "cmdecho" 0)
	(setq typ nil)
	(if (setq en (entsel "\nSelect Text entity on target layer: "))
		(setq
			ent (entget (car en))
			lay (cdr (assoc 8 ent))
			typ (cdr (assoc 0 ent))
		)
	)
	(if (= typ "TEXT")
		(setq ss (ssget "x" (list '(0 . "text") (cons 8 lay))))
	)
	(if ss
		(progn
			(setvar "highlight" 0)
			(setq
				strlist (list)
				sslen (sslength ss)
				idx 0
			);setq
			(repeat sslen
				(setq
					str (cdr (assoc 1 (entget (ssname ss idx))))
					strlist (if (member str strlist) strlist (append strlist (list str)))
					idx (1+ idx)
				);setq
			);repeat sslen
			(setq
				strlist (acad_strlsort strlist)
				stramnt (length strlist)
				idx 0
			);setq
			(princ (strcat "\n\n\n======= " (strcase lay) " =======\n")) 
			(repeat stramnt
				(cmd "select" ss "")
				(setq
					str (nth idx strlist)
					amt (rtos (sslength (ssget "p" (list (cons 1 str)))) 2 0)
					idx (1+ idx)
				);setq
				(princ (strcat "\n" str "\t" amt))
			);repeat stramnt
		);progn, if true.
	);if text objects...
	(textscr)
	(setvar "highlight" 1)
	(setvar "cmdecho" 1)
	(princ)
)

(princ "loaded. Start as C:CNT")
(princ)


