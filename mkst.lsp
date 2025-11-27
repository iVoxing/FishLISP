; 2025-11-25	optimization
; 2004-08-16

(defun c:mkst ()
	(if (setq st_num (getint "\n踏步数："))
		(progn
			(cond
				((or (<= st_num 0) (>= stnum 20))
					(alert "u R crazy >_<")
					(setq stbkname nil)
				)
				((and (> st_num 0) (< st_num 10))
					(setq stbkname (strcat "st0" (rtos st_num 2 0)))
				)
				((and (>= stnum 10) (< st_num 20))
					(setq stbkname (strcat "st" (rtos st_num 2 0)))
				)
			)
			(if stbkname
				(if (tblsearch "block" stbkname)
					(setq skbkname nil)
				)
			)
			(if stbkname
				(progn
					(setq st_hig (/ 1.0 st_num))
					(entmake (list 
						(cons 2 stbkname)
						'(10 0.0 0.0 0.0)
					))
					(fl_make_line (list 0.0 0.0 0.0) (list 0.0 st_hig 0.0))
					(setq idx 1)
					(repeat (1- st_num)
						(mapcar 'fl_make_line
							(list (list (1- idx) (* st_hig idx) 0.0) (list idx (* st_hig idx) 0.0))
							(list (list idx (* st_hig idx) 0.0) (list (1+ idx) (* st_hig (1+ idx)) 0.0))
						)
						(setq idx (1+ idx))
					)
					(entmake "endblk")
					(setq 
						ins_wid (getreal "\n踏宽：")
						ins_hig (getreal "\n梯段高度：")
					)
					(cmd "_insert" stbkname "x" ins_wid "y" ins_hig "r" "0" pause)
				)
			)
		)
	)
	(princ)
)
