; FishLISP
; 清除双线交叉
; 2025-11-21	optimization
; 2015-05-18	用新思路重寫，加強list級別的操作
;				用mapcar直接對在list級別獲得交點
;				用vl-sort的function比較法

(defun c:xc (/ ss_lines ss_amt n en_line list_lines ent_line pt1_line pt2_line list_int list_newpt m new_line)
	(prompt "\n选择多条交叉线: ")
	(setq ss_lines (ssget '((0 . "LINE"))))
	(if ss_lines (setq ss_amt (sslength ss_lines)))
	(setq 
		n 0 
		list_lines (list)
	)
	(repeat ss_amt
		(setq
			en_line (ssname ss_lines n)
			list_lines (append list_lines (list en_line))
			n (1+ n)
		)
		(redraw en_line 2)
	)
	(setq n 0)
	(repeat ss_amt
		(setq 
			en_line	 (nth n list_lines)
			ent_line (entget en_line)
			pt1_line (cdr (assoc 10 ent_line))
			pt2_line (cdr (assoc 11 ent_line))
			list_int (vl-remove en_line list_lines)
			list_int (mapcar 
						'(lambda (x_ / xpt1 xpt2)
							(setq xpt1 (cdr (assoc 10 (entget x_))) xpt2 (cdr (assoc 11 (entget x))))
							(inters pt1_line pt2_line xpt1 xpt2)
						)
						list_int
					)
			list_int 	(vl-remove nil list_int)
			list_int 	(append (list pt1_line pt2_line) list_int)
			list_newpt 	(vl-sort list_int (function (lambda (x_ y_) (< (car x_) (car y_)))))
			list_newpt 	(vl-sort list_newpt (function (lambda (x y) (< (cadr x_) (cadr y_)))))
	 	 	n (1+ n) 
			m 0
		)
		(repeat (/ (length list_newpt) 2)
			(setq 
				new_line (subst (cons 10 (nth m list_newpt)) (assoc 10 ent_line) ent_line)
				m (1+ m)
				new_line (subst (cons 11 (nth m list_newpt)) (assoc 11 new_line) new_line)
				m (1+ m)
			)
			(entmake new_line)
		)
	)
	(mapcar 'entdel list_lines)
	(redraw)
	(princ)
) 

(princ "loaded. Start as C:XC ")
(princ)