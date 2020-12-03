; just a practice

(defun c:fractal (/ en)
	(if (setq en (entsel "\nSelect a LINE object: "))
		(progn
			(initget "1 2 3 4")
			(setq key (getkword "\nSelect method: [1/2/3/4] "))
			(setq fac 0.618)
			(cond
				((= key "1")
					(mapcar 'frc1 (list (car en)))
				)
				((= key "2")
					(mapcar 'frc2 (list (car en)))
				)
				((= key "3")
					(mapcar 'frc3 (list (car en)))
				)
				((= key "4")
					(mapcar 'frc4 (list (car en)))
				)
			);cond
		);progn
);if
(princ)
);defun

(defun make_line (pt_1 pt_2 / en)
	(setq 
		en (entmake (list '(0 . "line") (cons 10 pt_1) (cons 11 pt_2)))
		en (entlast)
	)
	en
)

(defun frc1 (line_en / line_list)
	(setq
		line_list (list)
		ent (entget line_en)
		pt1 (cdr (assoc 10 ent))
		pt2 (cdr (assoc 11 ent))
		an1 (angle pt1 pt2)
		an2 (+ an1 (/ pi 2.0))
		dis (/ (distance pt1 pt2) 2.0)
		fac (* fac 1)
		pt3 (polar pt1 an1 dis)
		pt4 (polar pt3 an2 (* dis fac))
		line_list (mapcar 'make_line (list pt1 pt4) (list pt4 pt2))
	);setq
	(entdel line_en)
	(redraw)
	(if (> dis 1)
		(mapcar 'frc1 line_list)
	)
	(princ)
);defun

(defun frc2 (line_en / line_list)
	(setq 
		line_list (list)
		ent (entget line_en)
		pt1 (cdr (assoc 10 ent))
		pt2 (cdr (assoc 11 ent))
		an1 (angle pt1 pt2)
		an2 (+ an1 (/ pi 2.0))
		dis (distance pt1 pt2)
		pt3 (polar pt1 an1 (/ dis 3.0))
		pt4 (polar pt1 an1 (* dis (/ 2.0 3.0)))
		pt5 (polar pt3 an2 (/ dis 3.0))
		pt6 (polar pt4 an2 (/ dis 3.0))
		line_list (mapcar 'make_line (list pt1 pt5 pt6) (list pt5 pt6 pt2))
	);setq
	(entdel line_en)
	(redraw)
	(if (> dis 1)
		(mapcar 'frc2 line_list)
	)
	(princ)
);defun

(defun frc3 (line_en / line_list)
	(setq 
		line_list (list)
		ent (entget line_en)
		pt1 (cdr (assoc 10 ent))
		pt2 (cdr (assoc 11 ent))
		an1 (angle pt1 pt2)
		an2 (+ an1 (/ pi 2.0))
		dis (distance pt1 pt2)
		pt3 (polar pt1 an1 (/ dis 3.0))
		pt4 (polar pt1 an1 (* dis (/ 2.0 3.0)))
		pt5 (polar pt3 an2 (/ dis 2.8))
		pt6 (polar pt4 an2 (/ dis -3.5))
		line_list (mapcar 'make_line (list pt1 pt5 pt6) (list pt5 pt6 pt2))
	);setq
	(entdel line_en)
	(redraw)
	(if (> dis 1)
		(mapcar 'frc3 line_list)
	)
	(princ)
);defun

(defun frc4 (line_en / line_list)
	(setq 
		line_list (list)
		ent (entget line_en)
		pt1 (cdr (assoc 10 ent))
		pt2 (cdr (assoc 11 ent))
		an1 (angle pt1 pt2)
		an2 (+ an1 (/ pi 2.0))
		dis (distance pt1 pt2)
		pt3 (polar pt1 an1 (/ dis 2.0))
		pt4 (polar pt1 an1 (/ dis 3.0))
		pt5 (polar pt1 an1 (* dis (/ 2.0 3.0)))
		pt6 (polar pt3 an2 (* dis 0.288))
		line_list (mapcar 'make_line (list pt1 pt4 pt6 pt5) (list pt4 pt6 pt5 pt2))
	);setq
	(entdel line_en)
	(redraw)
	(if (> dis 1)
		(mapcar 'frc4 line_list)
	)
	(princ)
);defun

(princ "loaded. Start as C:FRACTAL ")
(princ)