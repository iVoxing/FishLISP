
(defun c:hcx (/ cl hc_sc)
	(setq cl (getvar "clayer"))
	(setvar "cmdecho" 0)
	(cmd "layer" "make" "col-h" "")
	(setq hc_sc (/ 1000 (* (getvar "dimscale") (getvar "ltscale"))))
	(if (setq ss (ssget '((8 . "col*"))))
		(cmd "bhatch" "p" "ansi31" "20" "0" "s" ss "" "")
	)
	(setvar "clayer" cl)
	(setvar "cmdecho" 1)
	(princ)
)

(defun c:hcc (/ cl hc_sc)
	(setq cl (getvar "clayer"))
	(setvar "cmdecho" 0)
	(cmd "layer" "make" "col-h" "")
	(setq hc_sc (/ 1000 (* (getvar "dimscale") (getvar "ltscale"))))
	(if (setq ss (ssget '((8 . "col*,pdim*"))))
		(cmd "bhatch" "p" "ar-conc" "0.5" "0" "s" ss "" "")
	)
	(setvar "clayer" cl)
	(setvar "cmdecho" 1)
	(princ)
)

(princ "loaded. C:HCX")
(princ)
