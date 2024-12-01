; FishLISP
; Multiple fillet
; 2020-11-18 v1.0

(set 'cmd (if (type command-s) command-s command))

(defun fl_undo_begin (/ cmdstat) 
	(setq olderr *error*)
	(defun *error* (s)
		(if cmdstat (setvar "cmdecho" cmdstat))
		(setq *error* olderr olderr nil)
		(gc)
	)
	(setq cmdstat (getvar "cmdecho"))
	(setvar "cmdecho" 0)
	(if (fl_check_ver 12) 
		(cmd "undo" "group")
		(cmd "undo" "begin")
	)
)

(defun fl_undo_end (/ cmdstat) 
	(if cmdstat nil (setq cmdstat (getvar "cmdecho")))
	(setvar "cmdecho" 0)
	(cmd "undo" "end")
	(setvar "cmdecho" cmdstat)
	(gc)
)

(defun c:mfr (/ loop k old_rad new_rad line_ss point_list join_list n line_ent c pt ss)
	(fl_undo_begin)
	(setq loop 1)
	(while loop
		(setq old_rad (getvar "filletrad"))
		(initget "Radius Select")
		(prompt(strcat "\n[Radius: " (rtos old_rad 2 2) "]<Select #-CROSS lines>: "))
		(setq
			k (getkword)
			k (if k k "Select")
		)
		(if (= k "Radius")
			(progn
				(prompt (strcat "\nRadius: <" (rtos old_rad 2 2) ">"))
				(setq new_rad (getreal))
				(setq new_rad (if new_rad new_rad old_rad))
				(setvar "filletrad" new_rad)
			)
			(setq loop nil)
		)
	)
	(setq line_ss (ssget '((0 . "line"))))
	(if line_ss
		(progn
			(setq
				point_list	(list)
				join_list	(list)
				n 0
			)
			(repeat (sslength line_ss)
				(setq line_ent (entget (ssname line_ss n)))
				(foreach c '(10 11)
					(setq pt (cdr (assoc c line_ent)))
					(if (member pt point_list)
						(setq join_list (append join_list (list pt)))
						(setq point_list (append point_list (list pt)))
					)
				)
				(setq n (1+ n))
			)
			(if join_list
				(mapcar
					'(lambda (pt)
						(setq ss (ssget "x" (list '(-4 . "<or")
							'(-4 . "<and") '(0 . "line") (cons 10 pt) '(-4 . "and>")
							'(-4 . "<and") '(0 . "line") (cons 11 pt) '(-4 . "and>")
						'(-4 . "or>"))))
						(cmd "_.fillet" (ssname ss 0) (ssname ss 1))
					)
					join_list
				)
				(princ "\nNo joioner found.")
			)
		);progn
		(princ "\nNo line Selected.")
	);line_ss
	(fl_undo_end)
	(princ)
)

(princ "\nFishLISP C:MFR ")
(princ)
