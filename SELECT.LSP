(defun ss_uni (ss1_ ss2_)
	(cond
		((not ss1_) ss2_)
		((not ss2_) ss1_)
		((and (not ss1_) (not ss2_)))
		(t
			(setvar "cmdecho" 0)
			(cmd "_select" ss1_ ss2_ "")
			(setvar "cmdecho" 1)
			(ssget "p")
		)
	)
)

(defun ss_sub (ss1_ ss2_)
	(cond 
		((not ss1_))
		((not ss2_) ss1_)
		(t
			(setvar "cmdecho" 0)
			(cmd "_select" ss1_ "r" ss2_ "")
			(setvar "cmdecho" 1)
			(ssget "p")
		)
	)
)

(defun ss_int (ss1_ ss2_)
	(ss_uni
		(ss_sub ss1_ (ss_sub ss1_ ss2_))
		(ss_sub ss2_ (ss_sub ss2_ ss1_))
	)
)
 
 