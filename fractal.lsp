; just a practice

(defun c:fractal (/ en)
	(if (setq en (entsel "\nSelect a LINE object: "))
		(progn
			(initget "1 2 3 4")
			(setq key (getkword "\nSelect method: 1[2/3/4] "))
			(setq key (if key key "1"))
			(if fl_bench_begin (fl_bench_begin))
			(mapcar (read (strcat "frc" key)) (list (car en)))
			(if fl_bench_end (fl_bench_end))
		);progn
	);if
	(princ)
);defun

(defun make_line (pt_1 pt_2)
	(entmake (list '(0 . "line") (cons 10 pt_1) (cons 11 pt_2)))
	(entlast)
)

(defun base_line_treat ()
	(setq
		ent (entget line_en)
		pt1 (cdr (assoc 10 ent))
		pt2 (cdr (assoc 11 ent))
		an1 (angle pt1 pt2)
		an2 (+ an1 (/ pi 2.0))
		dis (distance pt1 pt2)
	)
)

(defun post_treat ()
	(entdel line_en)
	(redraw)
	(if (> dis 1)
		(mapcar (read frc) line_list)
	)
)

(defun frc1 (line_en)
	(setq frc "frc1")
	(base_line_treat)
	(setq
		fac 0.3
		pt3 (polar pt1 an1 (/ dis 2.0))
		pt4 (polar pt3 an2 (* dis fac))
		line_list (mapcar 'make_line (list pt1 pt4) (list pt4 pt2))
	);setq
	(post_treat)
);defun

(defun frc2 (line_en)
	(setq frc "frc2")
	(base_line_treat)
	(setq 
		pt3 (polar pt1 an1 (/ dis 3.0))
		pt4 (polar pt1 an1 (* dis (/ 2.0 3.0)))
		pt5 (polar pt3 an2 (/ dis 3.0))
		pt6 (polar pt4 an2 (/ dis 3.0))
		line_list (mapcar 'make_line (list pt1 pt5 pt6) (list pt5 pt6 pt2))
	);setq
	(post_treat)
);defun

(defun frc3 (line_en)
	(setq frc "frc3")
	(base_line_treat)
	(setq 
		pt3 (polar pt1 an1 (/ dis 3.0))
		pt4 (polar pt1 an1 (* dis (/ 2.0 3.0)))
		pt5 (polar pt3 an2 (/ dis 2.8))
		pt6 (polar pt4 an2 (/ dis -3.5))
		line_list (mapcar 'make_line (list pt1 pt5 pt6) (list pt5 pt6 pt2))
	);setq
	(post_treat)
);defun

(defun frc4 (line_en)
	(setq frc "frc3")
	(base_line_treat)
	(setq 
		pt3 (polar pt1 an1 (/ dis 2.0))
		pt4 (polar pt1 an1 (/ dis 3.0))
		pt5 (polar pt1 an1 (* dis (/ 2.0 3.0)))
		pt6 (polar pt3 an2 (* dis 0.288))
		line_list (mapcar 'make_line (list pt1 pt4 pt6 pt5) (list pt4 pt6 pt5 pt2))
	);setq
	(post_treat)
);defun

(princ "loaded. Start as C:FRACTAL ")
(princ)