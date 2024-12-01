(defun c:ppp ()
	(setq 
		plot_ss (ssget "x" (list (cons 8 "plotaaa")))
		idx 0
	)
	(if plot_ss
		(progn
			(repeat (sslength plot_ss)
				(setq 
					line_ent (entget (ssname plot_ss idx))
					line_pt1 (cdr (assoc 10 line_ent))
					line_pt2 (cdr (assoc 11 line_ent))
					idx (1+ idx)
				);setq
				(cmd "plot" "y" "" "" "" "m" "l" "" "w" line_pt1 line_pt2 "f" "" "" "" "" "" "n" "n" "y")
			);repeat
		);progn
		(alert "没有预设打印范围")
	);if plot_ss
	(princ)
)

(princ)