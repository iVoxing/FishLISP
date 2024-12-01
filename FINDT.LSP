; History
; V3.1	2003-07-02	增加*ERROR*处理
; 	2003-06-19	更改命令名称为C:FFT
; V3.0	2001-11-26	增加了缺省值和替换功能。
; V2.0	2001-03-15	更加简洁的处理，重写了Zoom部分和比例设置部分。
; V1.1	
; V1.0	1996-12-23

(defun findt (ss / idx cnt key bsd)
	(setvar "cmdecho" 0)
	(setq idx 0)
	(setq cnt (sslength ss))
	(while (< idx cnt)
		(setq
			text_en (ssname ss idx)
			ent (entget text_en)
			pt0 (cdr (assoc 10 ent))
			str (cdr (assoc 1 ent))
		);setq
		(cmd "_zoom" "c" pt0 z_sc)
		(getkey)
	);while
	(if bsd nil (setq bsd cnt))
	(princ (strcat "\n" or_str " >> " (rtos cnt 2 0) " found, " (rtos bsd 2 0) " browsed. "))
	(setvar "cmdecho" 1)
)

(defun getkey ()
	(initget "Next Continue Zoom Replace Exit")
	(setq key (getpoint "\n[Continue/Zoom height/Replace/Exit]<Next>: "))
	(if key nil (setq key "Next"))
	(cond
		((listp key)
			(setq 
				bsd (1+ idx)
				ft_lastidx (1- idx)
				idx cnt
			)
		)
		((= key "Next")
			(setq 
				ft_lastidx idx
				idx (1+ idx)
			)
		)
		((= key "Continue")
			(setq idx (1+ ft_lastidx))
		)
		((= key "Zoom")
			(setsc)
			(getkey)
		)
		((= key "Exit")
			(setq 
				bsd (1+ idx)
				ft_lastidx (1- idx)
				idx cnt
			)
		)
		((= key "Replace")
			(ft_rp)
			(setq 
				ft_lastidx idx
				idx (1+ idx)
			)
		)
	)
)

(defun ft_rp (/ n_str)
	(if ft_new_str nil (setq ft_new_str ""))
	(princ (strcat "\nNew String: <" ft_new_str ">"))
	(setq n_str (getstring 1))
	(if (= n_str "") (setq n_str ft_new_str))
	(if (= n_str "")
		(ft_rp)
		(progn
			(setq ft_new_str n_str)
			(entmod (subst (cons 1 n_str) (assoc 1 ent) ent))
		)
	)
)

(defun setsc ()
	(princ (strcat "\nZoom height: <" z_sc ">: "))
	(setq 
		z_sc (getint)
		z_sc (if z_sc (rtos z_sc) ft_z_sc)
	)
	(setq ft_z_sc z_sc)
)

(defun c:fft (/ str ss z_sc ent olderr)
	(setq olderr *error*)
	(defun *error* (s)
		(setvar "highlight" 1)
		(setvar "cmdecho" 1)
		(setq *error* olderr olderr nil)
		(princ)
	)
	(prompt "\nFishLISP, Dec 23 1996.")
	(prompt "\nFind text, ver 3.0, start as C:FFT")
	(setvar "highlight" 0)
	(if ft_z_sc nil (setq ft_z_sc "10000"))
	(if ft_dft_str nil (setq ft_dft_str ""))
	(if ft_lastidx nil (setq ft_lastidx 0))
	(setq z_sc ft_z_sc)
	(ft_get)
	(setq ft_dft_str or_str)
	(setq str (if ft_cas (strcase or_str) OR_STR))
	(setq ss (ssget "X" (list '(0 . "text") (cons 1 str))))
	(setvar "highlight" 1)
	(if ss (findt ss) (princ "Text not found."))
	(princ)
)

(defun ft_get ()
	(princ (strcat "\nText to search: <" ft_dft_str "> "))
	(setq or_str (getstring1))
	(if (= or_str "") (setq or_str ft_dft_str))
	(if (= or_str "") (ft_get))
)

(princ "loaded. Start as C:FFT")
(princ)
