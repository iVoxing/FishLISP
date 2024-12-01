(defun c:ltl (/ olderr ss amt idx len t_len pt1 pt2)
	(setq olderr *error*)
	(defun *error* (s)
		(setq oderr nil ss nil amt nil idx nil len nil t_len nil pt1 nil pt2 nil)
		(setq *error* olderr olderr nil)
		(princ)
	)
	(setq ss (ssget '((0 . "line"))))
	(if ss
		(setq 
			amt (sslength ss)
			idx 0
			t_len 0.0
		)
		(setq amt 0)
	)
	(repeat amt
		(setq 
			ent (entget (ssname ss idx))
			pt1 (cdr (assoc 10 ent))
			pt2 (cdr (assoc 11 ent))
			len (distance pt1 pt2)
			t_len (+ t_len len)
			idx (1+ idx)
		)
	)
	(princ
		(strcat "\n" (rtos amt 2 0) " line(s) found. Total length=" (rtos t_len) ". ")
	)
	(setq *error* olderr)
	(princ)
	)

	(defun c:bta (/ olderr lay ss amt idx en t_area t_peri)
	(setq olderr *error*)
	(defun *error* (s)
		(setq oderr nil ss nil amt nil idx nil t_area nil lay nil t_peri nil)
		(setq *error* olderr olderr nil)
		(princ)
	)
	(setvar "cmdecho" 0)
	(while (not lay)
		(setq bta_lay (if bta_lay bta_lay "0"))
		(princ (strcat "\nTarget layer: <" bta_lay "> "))
		(setq lay (getstring))
		(if (= lay "") (setq lay bta_lay))
		(if (tblsearch "layer" lay)
			nil
			(progn
				(princ "** Layer not exist! **")
				(setq lay nil)
			)
		)
	)
	(setq bta_lay lay)
	(setq ss (ssget (list (cons 8 lay) '(0 . "lwpolyline,region") '(70 . 1))))
	(if ss (setq amt (sslength ss) idx 0) (setq amt 0))
	(setq t_area 0.0 t_peri 0.0)
	(repeat amt
		(setq en (ssname ss idx))
		(cmd "area" "object" en)
		(setq t_area (+ t_area (getvar "area"))
			t_peri (+ t_peri (getvar "perimeter"))
		)
		(setq idx (1+ idx))
	)
	(princ (strcat 
		"\n" (rtos amt 2 0)
		" closed lwpolyline found. Total area="
		(rtos (/ t_area 1000000.0) 2 2) "M2."
		"Total perimeter="
		(rtos (/ t_peri 1000.0) 2 3) "M. "
	))
	(setvar "cmdecho" 1)
	(setq *error* olderr)
	(princ)
)

(princ "loaded. Start as C:LTL C:BTA ")
(princ)

