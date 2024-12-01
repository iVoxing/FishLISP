(setq olderr *error*)
(defun *error* (s)
	(setq *error* olderr olderr nil)
)

(defun c:cut (/ del_list bnd_ss bnd_am cut_ss cut_id cut_en cut_ent cut_pt1 cut_pt2
				bnd_id intlist bnd_en bnd_ent bnd_pt1 bnd_pt2 int bnd_id intidx)
	(fishlisp "CUT" "3.0")
	(setvar "cmdecho" 0)
	(fl_undo_begin)
	(setq del_list (list))
	(princ "\n选择剪切线：")
	(if (setq bnd_ss (ssget '((0 . "line"))))
		(if (> (setq bnd_am (sslength bnd_ss)) 1)
			(progn
				(princ "\n选择将被剪切的线：")
				(if (setq cut_ss (ssget '((0 . "line"))))
					(progn
						(setq
							cut_am (sslength cut_ss)
							cut_id 0
						);setq
						(repeat cut_am
							(setq
								cut_en (ssname cut_ss cut_id)
								cut_ent (entget cut_en)
								cut_pt1 (cdr (assoc 10 cut_ent))
								cut_pt2 (cdr (assoc 11 cut_ent))
								bnd_id 0
								intlist (list)
							);setq
							(repeat bnd_am
								(setq
									bnd_en (ssname bnd_ss bnd_id)
									bnd_ent (entget bnd_en)
									bnd_pt1 (cdr (assoc 10 bnd_ent))
									bnd_pt2 (cdr (assoc 11 bnd_ent))
									int (inters cut_pt1 cut_pt2 bnd_pt1 bnd_pt2)
									bnd_id (1+ bnd_id)
								);setq
								(if int (setq intlist (append intlist (list int))))
							);repeat bnd_am
							(if intlist
								(progn
									(setq
										intlist (append (list cut_pt1) (append intlist (list cut_pt2)))
										intlist (if (= (car cut_pt1) (car cut_pt2))
													(vl-sort intlist '(lambda (pt1 pt2) (< (cadr pt1) (cadr pt2))))
													(vl-sort intlist '(lambda (pt1 pt2) (< (car pt1) (car pt2))))
												)
										intidx 0
										del_list (append del_list (list cut_en))
									);setq
									(repeat (/ (length intlist) 2)
										(setq
											newent (subst (cons 10 (nth intidx intlist)) (assoc 10 cut_ent) cut_ent)
											newent (subst (cons 11 (nth (1+ intidx) intlist)) (assoc 11 newent) newent)
											intidx (+ intidx 2)
										);setq
										(entmake (cdr newent))
									);repeat to draw new lines
								);progn true
							);if intlist
							(setq cut_id (1+ cut_id))
						);repeat cut_am
					);progn true
				);if cut_ss
			);progn
		);if bnd_am > 1
	);if bnd_ss
	(if del_list (foreach itm del_list (entdel itm)))
	(setvar "cmdecho" 0)
	(fl_undo_end)
	(princ)
);defun c:cut

(princ "FishLISP C:CUT")
(princ)