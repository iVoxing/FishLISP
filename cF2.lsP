; 2001-12-04	v2.0	Á¬ÐøÑ¡Ôñline


(defun cf_get_dis (/ dis)
	(setq cf_dis (if cf_dis cf_dis 0))
	(princ
		(strcat
			"\n["
			(if cf_mode "Line" "Pline")
			"/"
			(if cf_mid "Side" "Middle")
			"]Offset distance: <"
			(if (zerop cf_dis) "Through" (rtos cf_dis 2 2))
			"> "
		);strcat
	);princ
	(initget "Line Pline Middle Side Through")
	(setq dis (getdist))
	(setq dis (if dis dis cf_dis))
	(cond
		((null dis)
		)
		((= dis "Line")
			(setq cf_mode nil)
			(cf_get_dis)
		)
		((= dis "Pline")
			(setq cf_mode 1)
			(cf_get_dis)
		)
		((= dis "Middle")
			(setq cf_mid 1)
			(cf_get_dis)
		)
		((= dis "Side")
			(setq cf_mid nil)
			(cf_get_dis)
		)
		((= dis "Through")
			(setq dis 0)
			dis
		)
		( t
			dis
		)
	);cond
)

(defun cf_main (/ dis en0 pt0 et0 ln0 pt01 pt02 en1 ln1 pt11 pt12)
	(setq dis (cf_get_dis))
	(setq cf_dis dis)
	(while (setq en0 (entsel "\nPick a line to edit: "))
		(setq 
			pt0 (cadr en0)
			et0 (car en0)
			ln0 (entget et0)
			pt01 (cdr (assoc 10 ln0))
			pt02 (cdr (assoc 11 ln0))
		)
		(cmd "offset" (if (zerop dis) "t" dis) pt0 pause "")
		(setq 
			en1 (entlast)
			ln1 (entget en1)
			pt11 (cdr (assoc 10 ln1))
			pt12 (cdr (assoc 11 ln1))
		)
		(cmd "line" pt01 pt11 "")
		(cmd "line" pt02 pt12 "")
		(if cf_mode
			(princ "\nModule not ready :) ")
		)
		(if cf_mid
			(princ "\nModule not ready :) ")
		)
	);while
);defun cf_main

(defun c:cf ()
	(princ "\nOffset line to Rectangle, Ver2.0.BETA")
	(cf_main)
	(princ)
)

(princ "loaded. Start as C:CF")
(princ)
