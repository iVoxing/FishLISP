; 2000-06-05调用新的Sort函数；
; 增加错误自陷；
; 改进界面。

(if (findfile "function.lsp")
	(load "function")
	(exit)
)

(defun c:dimlin (/ ss amnt pt_base pt_ext olderr)
	(cssave)
	(prompt "\nSelect lines to dim: ")
	(setq ss (ssget '((0 . "LINE"))))
	(if ss (setq amnt (sslength ss)))
	(if (> amnt 1)
		(setq pt_base (getpoint "\nLocation of base line: "))
	)
	(if pt_base (setq pt_ext (getpoint pt_base "\nLocation of extension lines: ")))
	(if pt_ext (dimlin))
	(csrestore)
	(setq *error* olderr olderr nil)
	(princ)
)

(defun dimlin (/ ent pt1 pt2 ang y_list idx oo os cl ucsf)
	(setq 
		oo (getvar "orthomode")
		os (getvar "osmode")
		cl (getvar "clayer")
		ucsf (getvar "ucsfollow")
	)
	(setvar "cmdecho" 0)
	(setvar "blipmode" 0)
	(setvar "orthomode" 0)
	(setvar "osmode" 0)
	(setvar "ucsfollow" 0)
	(setvar "ucsicon" 0)
	(cmd "_layer" "m" FLLT_DIM0 "c" "g" "" "")
	(setq 
		ent (entget (ssname ss 0))
		pt1 (cdr (assoc 10 ent))
		pt2 (cdr (assoc 11 ent))
		ang (/ (* (angle pt1 pt2) 180.0) pi)
	)
	(if (and (> ang 45.0) (< ang 225.0)) (setq ang (rem (+ ang 180.0) 360.0)))
	(cmd "_ucs" "z" ang)
	(setq 
		y_list (list) idx 0
		pt_ext (trans pt_ext 0 1)
		pt_base (trans pt_base 0 1)
	)
	(repeat amnt
		(setq y_list (append y_list (list (cadr (trans (cdr (assoc 10 (entget (ssname ss idx)))) 0 1)))))
		(setq idx (1+ idx))
	)
	(setq y_list (vl-sort y_list '<) idx 0)
	(repeat (1- amnt)
		(setq 
			pt1 (list (car pt_ext) (nth idx y_list))
			pt2 (list (car pt_ext) (nth (setq idx (1+ idx)) y_list))
		)
		(cmd "_dim1" "ver" pt1 pt2 pt_base "")
	)

	(setvar "cmdecho" 1)
	(setvar "orthomode" oo)
	(setvar "osmode" os)
	(setvar "clayer" cl)
	(setvar "blipmode" 1)
	(setvar "ucsfollow" ucsf)
	(setvar "ucsicon" 1)
)

(defun c:dl () (c:dimlin) (princ))

(princ "\loaded. Start as C:DIMLIN or C:DL")
(princ)
