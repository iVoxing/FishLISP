
(defun skip-o-p (char-code_)
	;; 如果字符码是 #\O (ASCII 79)，返回 T
	(= char-code_ 79)
)

(defun int-to-axis (n_ / result char-code)
	;; 将整数 n_ 转换为轴线号（类似Excel列名，但跳过含O的）
	(setq result "")
	(while (> n_ 0)
		(setq n_ (1- n_)) ; 转为0基索引
		(setq char-code (+ 65 (rem n_ 26)))
		(if (skip-o-p char-code)
		(progn
			;; 如果当前位是O，跳过它，进位处理
			(setq n_ (1+ n_)) ; 回退
			(setq n_ (+ n_ 26)) ; 进位到下一个字母
		)
		(progn
			(setq result (strcat (chr char-code) result))
			(setq n_ (/ n_ 26))
		)
		)
	)
	result
)

(defun generate-axis-labels (count_ / i labels)
	;; 生成指定数量的轴线号，跳过含O的
	(setq i 0
		labels '()
		n 0
	)
	(while (< i count_)
		(setq axis (int-to-axis (setq n (1+ n))))
		(if (not (vl-string-search "O" axis))
		(progn
			(setq labels (cons axis labels))
			(setq i (1+ i))
		)
		)
	)
	(reverse labels)
)

;; 示例：生成前50个纵向轴线号
(defun c:GENAXIS ()
	(setq lst (generate-axis-labels 50))
	(foreach label lst
		(princ (strcat label "\n"))
	)
	(princ)
)