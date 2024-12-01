(defun ss_uni (ss1 ss2)
	(cond
		((not ss1) ss2)
		((not ss2) ss1)
		((and (not ss1) (not ss2)))
		(t
			(setvar "cmdecho" 0)
			(cmd "_select" ss1 ss2 "")
			(setvar "cmdecho" 1)
			(ssget "p")
		)
	)
)

(defun ss_sub (ss1 ss2)
	(cond 
		((not ss1))
		((not ss2) ss1)
		(t
			(setvar "cmdecho" 0)
			(cmd "_select" ss1 "r" ss2 "")
			(setvar "cmdecho" 1)
			(ssget "p")
		)
	)
)

(defun ss_int (ss1 ss2)
	(ss_uni
		(ss_sub ss1 (ss_sub ss1 ss2))
		(ss_sub ss2 (ss_sub ss2 ss1))
	)
)
 
 