;
;
(defun c:lca (/ ori_pt mysnap x0 y0 z0 x1 y1 z1 x2 y2 z2 line_obn line_obl line_ss line_idx)
	(setvar "cmdecho" 0)
	(cmd "undo" "begin")
;����ԭ�㣬ȱʡ0,0
	(setq lcab:ori_pt (if lcab:ori_pt lcab:ori_pt '(0 0 0)))
	(princ (strcat
	"\n���û�׼�㣺<"
		(rtos (car lcab:ori_pt))
		","
		(rtos (cadr lcab:ori_pt))
		"> "
	));princ
	(setq ori_pt (getpoint))
	(setq ori_pt (if ori_pt ori_pt lcab:ori_pt))
	(setq lcab:ori_pt ori_pt)
	(setq 
		x0 (car lcab:ori_pt)
		y0 (cadr lcab:ori_pt)
		z0 (caddr lcab:ori_pt)
	)
	;����У׼���ȣ���ģ����ȱʡ10
	(setq lcab:snap (if lcab:snap lcab:snap 10))
	(princ "\n����У׼���ȣ�<")
	(princ lcab:snap)
	(setq mysnap (getint "> "))
	(setq mysnap (if mysnap mysnap lcab:snap))
	(setq lcab:snap mysnap)
	;�ռ�lineͼԪ
	(prompt "\nѡ��ͼԪ��")
	(setq line_ss (ssget '((0 . "line, lwployline"))))
	(if line_ss
		(progn
			(setq 
				modi_idx 0
				line_idx 0
			)
			(repeat (sslength line_ss)
				(setq 
					line_obn (ssname line_ss line_idx)
					line_obl (entget line_obn)
					line_pt1 (cdr (assoc 10 line_obl))
					line_pt2 (cdr (assoc 11 line_obl))
					x1 (car line_pt1)
					x2 (car line_pt2)
					y1 (cadr line_pt1)
					y2 (cadr line_pt2)
					z1 (caddr line_pt1)
					z2 (caddr line_pt2)
				);setq
				(if (= (rem (- x1 x0) lcab:snap)
						(rem (- x2 x0) lcab:snap)
						(rem (- y1 y0) lcab:snap)
						(rem (- y2 y0) lcab:snap)
						(rem (- z1 z0) lcab:snap)
						(rem (- z2 z0) lcab:snap)
						0.0
					)
					nil
					(progn
						;��¼�辭У����ͼԪ
						(setq modi_idx (1+ modi_idx))
						;У��lineͼԪ
						(setq 
							x1 (+ x1 (/ lcab:snap 2.0))
							x1 (- x1 (rem (- x1 x0) lcab:snap))
							x2 (+ x2 (/ lcab:snap 2.0))
							x2 (- x2 (rem (- x2 x0) lcab:snap))
							y1 (+ y1 (/ lcab:snap 2.0))
							y1 (- y1 (rem (- y1 y0) lcab:snap))
							y2 (+ y2 (/ lcab:snap 2.0))
							y2 (- y2 (rem (- y2 y0) lcab:snap))
							z1 (+ z1 (/ lcab:snap 2.0))
							z1 (- z1 (rem (- z1 z0) lcab:snap))
							z2 (+ z2 (/ lcab:snap 2.0))
							z2 (- z2 (rem (- z2 z0) lcab:snap))
						)
						(entmod (setq line_obl (subst (cons 10 (list x1 y1 z1)) (assoc 10 line_obl) line_obl)))
						(entmod (setq line_obl (subst (cons 11 (list x2 y2 z2)) (assoc 11 line_obl) line_obl)))
					);progn
				);if
				(setq line_idx (1+ line_idx))
			);repeat
		);line_ss progn
	);if line_ss
	(cmd "undo" "end")
	(setvar "cmdecho" 1)
	(princ (strcat "\n" (rtos modi_idx 2 0) " Line(s) �Ѿ�У׼��"))
	(princ)
)

(princ)