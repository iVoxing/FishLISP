;FishLISP
;清除双线交叉
;	2015-05-18	用新思路重寫，加強list級別的操作
;				用mapcar直接對在list級別獲得交點
;				用vl-sort的function比較法

(set 'cmd (if (type command-s) command-s command))

;设置UNDO起点
(defun fl_undo_begin (/ cmdstat) 
	(setq cmdstat (getvar "cmdecho"))
	(setvar "cmdecho" 0)
	(if (= (substr (getvar "acadver") 1 2) "12") 
		(cmd "undo" "group")
		(cmd "undo" "begin")
	)
	(setvar "cmdecho" cmdstat)
)

;设置UNDO终点
;在命令结束和ERROR处理时，均可使用
(defun fl_undo_end (/ cmdstat) 
	(setq cmdstat (getvar "cmdecho"))
	(setvar "cmdecho" 0)
	(cmd "undo" "end")
	(setvar "cmdecho" cmdstat)
)

(defun c:xc (/ ss_lines ss_amt n en_line list_lines ent_line pt1_line pt2_line list_int xpt1 xpt2 list_newpt m new_line)
	(fl_undo_begin)
	(prompt "\n选择多条交叉线: ")
	(setq ss_lines (ssget '((0 . "line"))))
	(if ss_lines (setq ss_amt (sslength ss_lines)))
	(setq n 0 list_lines (list))
	(repeat ss_amt
		 (setq
		 	en_line (ssname ss_lines n)
			list_lines (append list_lines (list en_line))
			n (1+ n)
		 )
		 (redraw en_line 2)
	);; 获得LINE列表
	(setq n 0)
	(repeat ss_amt
		(setq 
			en_line	 (nth n list_lines)
			ent_line (entget en_line)
			pt1_line (cdr (assoc 10 ent_line))
			pt2_line (cdr (assoc 11 ent_line))
			list_int (vl-remove en_line list_lines)
			list_int (mapcar 
						'(lambda (x)
							(setq xpt1 (cdr (assoc 10 (entget x))) xpt2 (cdr (assoc 11 (entget x))));setq 
							(inters pt1_line pt2_line xpt1 xpt2);
						);lambda 将相交LINE转换为交点
						list_int
					);mapcar 获得交点列表
			list_int 	(vl-remove nil list_int); 清除无效点
			list_int 	(append (list pt1_line pt2_line) list_int); 交點列表再加入端点，形成全部點集
			list_newpt 	(vl-sort list_int (function (lambda (x y)(< (car x) (car y))))); vl-sort的function用法示例
			list_newpt 	(vl-sort list_newpt (function (lambda (x y)(< (cadr x) (cadr y)))));
	 	 	n (1+ n) m 0
		);setq
		(repeat (/ (length list_newpt) 2)
			(setq 
				new_line (subst (cons 10 (nth m list_newpt)) (assoc 10 ent_line) ent_line)
				m (1+ m)
				new_line (subst (cons 11 (nth m list_newpt)) (assoc 11 new_line) new_line)
				m (1+ m)
			)
			(entmake new_line)
		);repeat
	);repeat ss_amt
	(mapcar 'entdel list_lines);刪除原有實體
	(redraw)
	(fl_undo_end)
	(princ)
) 

(princ "loaded. Start as C:XC ")
(princ)