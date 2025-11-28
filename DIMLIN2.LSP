; 2000-06-05调用新的Sort函数；
; 增加错误自陷；
; 改进界面。

(if (findfile "function.lsp")
	(load "function")
	(exit (princ "\n Function module not found. "))
)

(defun c:dimlin2 (/ oo cl ucsf dsc ss amnt pt_base)
	(setq 
		oo (getvar "orthomode")
		cl (getvar "clayer")
		ucsf (getvar "ucsfollow")
		dsc (getvar "dimscale")
		dclr (getvar "dimclre")
	)

	; 注意 DIMCLRE 的使用，除非设为缺省值比如0
	; 须配合 DIMCLRD 0 使用
	(setvar "DIMCLRE" 134)
	(setvar "DIMCLRD" 0)

	(setvar "cmdecho" 0)
	(setvar "blipmode" 0)
	(setvar "orthomode" 0)
	(setvar "ucsfollow" 0)
	(setvar "clayer" FLLT_DIM0)

	(prompt "\nSelect lines to dim: ")
	(setq ss (ssget '((0 . "LINE"))))
	(if ss (setq amnt (sslength ss)))
	(if (> amnt 1) (setq pt_base (getpoint "\n标注点：")))
	(if pt_base (dimlin))

	(setvar "DIMCLRE" dclr)
	(setvar "DIMCLRD" 0)

	(setvar "orthomode" oo)
	(setvar "clayer" cl)
	(setvar "blipmode" 1)
	(setvar "ucsfollow" ucsf)
	(setvar "ucsicon" 1)
	(setvar "cmdecho" 1)

	(princ)
)

(defun dimlin (/ ent pt1 pt2 ang )
	(setq 
		ent (entget (ssname ss 0))
		pt1 (cdr (assoc 10 ent))
		pt2 (cdr (assoc 11 ent))
		ang (/ (* (angle pt1 pt2) 180.0) pi)
	)
	(if (and (> ang 45.0) (< ang 225.0)) 
		(setq 
			pttemp pt1 pt1 pt2 pt2 pttemp pttemp nil
		 	ang (rem (+ ang 180.0) 360.0)
		)
	)
	(setvar "ucsicon" 0)
	(cmd "_ucs" "z" ang)

	(setq y_list (list) idx 0)
	(setq x_00 (car (trans pt_base 0 1)))
	(if (>= (abs (- (car (trans pt1 0 1)) x_00)) (abs (- (car (trans pt2 0 1)) x_00)))
		(setq 
			x_01 (+ x_00 (* 10.0 dsc))
			x_02 (+ x_00 (* 20.0 dsc))
		)
		(setq 
			x_01 (- x_00 (* 10.0 dsc))
			x_02 (- x_00 (* 20.0 dsc))
		)
	)
	(repeat amnt
		(setq y_list (append y_list (list (cadr (trans (cdr (assoc 10 (entget (ssname ss idx)))) 0 1)))))
		(setq idx (1+ idx))
	)
	(setq 
		y_list (vl-sort y_list '<)
		idx 0
		y_00 (nth 0 y_list)
		pt01 (list x_00 y_00)
	)
	(repeat (1- amnt)
		(setq 
			pt1 (list x_00 (nth idx y_list))
			pt2 (list x_00 (nth (setq idx (1+ idx)) y_list))
		)
		(cmd "_dim1" "ver" pt1 pt2 (list x_01 y_00) "")
	)
	(setq pt02 pt2)
	(cmd "_dim1" "ver" pt01 pt02 (list x_02 y_00) "")

	(cmd "_ucs" "p" "")

)

(defun c:dl () (c:dimlin2) (princ))

(princ "\loaded. Start as C:DIMLIN or C:DL")
(princ)
