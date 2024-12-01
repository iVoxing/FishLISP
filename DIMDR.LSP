; ±ê×¢ÃÅµÄ³ß´ç
;
;
(defun dr_dim (ss / wid cnt ent ins scl ang pt txt)
	(setq wid (getvar "dimscale"))
	(setq cnt 0)
	(repeat (sslength ss)
		(setq
			ent (entget (ssname ss cnt))
			ins (cdr (assoc 10 ent))
			scl (abs (cdr (assoc 41 ent)))
			ang (abs (- (cdr (assoc 50 ent)) pi))
			pt(polar ins (- ang (/ pi 2.0)) (* 5.0 wid))
			txt (strcat "M-" (rtos (* scl 1000.0) 2 0))
		)
		(if (= ang pi) (setq ang 0.0))
		(entmake (list
			'(0 . "text") '(8 . "t") '(7 . "STANDARD") '(72 . 1) (cons 11 pt) (cons 10 pt) (cons 1 txt) (cons 50 ang) (cons 40 (* wid 3.0))
		))
		(setq cnt (1+ cnt))
	)
)

(defun c:dimdr ()
	(initget "All")
	(setq key (getkword "\n[All the doors] RETURN to select:"))
	(if (= key "All")
		 (setq ss (ssget "x" '((0 . "insert")(2 . "dr?"))))
		 (setq ss (ssget '((0 . "insert")(2 . "dr?"))))
	)
	(if ss (dr_dim ss))
	(princ)
)
