(defun list_pgp	()
	(if (setq pgp_file (findfile "acad.pgp"))
		(progn
			(setq 
				pgp_r (open pgp_file "r")
				cmd_list nil
				addtolist nil
			)
			(while (setq pgp_l (read-line pgp_r))
				(if (= ";-- Sample aliases for AutoCAD commands --" pgp_l)
					(setq addtolist t)
				)
				(cond
					((= "" pgp_l) nil)
					((= (substr pgp_l 1 1) ";") nil)
					((= ";in AutoCAD Release 13." pgp_l)
						(setq addtolist nil)
					)
					(t
						(if addtolist (princ (strcat "\n" pgp_l)))
					)
				)
			)
			(close pgp_r)
		)
		(alert "\nACAD.PGPŒ¥’“µΩ£°")
	)
	(princ)
	)

	(defun list_cmd	()
	(if (setq pgp_file (findfile "acad.pgp"))
		(progn
			(setq 
				pgp_r (open pgp_file "r")
				cmd_list nil
				addtolist nil
			)
			(while (setq pgp_l (read-line pgp_r))
				(if (= ";-- Sample aliases for AutoCAD commands --" pgp_l)
					(setq addtolist t)
				)
				(cond
					((= "" pgp_l) nil)
					((= ";in AutoCAD Release 13." pgp_l)
						(setq addtolist nil)
					)
					((= (substr pgp_l 1 1) ";") nil)
					(t
						(if addtolist
							; (princ (strcat "\n" (substr pgp_l 1 (vl-string-search "," pgp_l))))
							(setq cmd_list (append cmd_list (list (substr pgp_l 1 (vl-string-search "," pgp_l)))))
						)
					)
				)
			)
			(close pgp_r)
		)
		(alert "\nACAD.PGPŒ¥’“µΩ£°")
	)
	(princ)
)

(defun c:sss () (list_cmd) (princ))

(princ)