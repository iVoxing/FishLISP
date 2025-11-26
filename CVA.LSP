; 将离散的轴线号转换成Insert，临时工具
; 2025-11-26	日常维护
; 1998-09-09	

(defun c:cva (/ c_ss c_amt c_idx ss_del c_en c_ent c_cen c_rad pt1 pt2 t_ss t_en scl att axi cl os)
	(setvar "cmdecho" 0)
	(setq cl (getvar "clayer"))
	(setq os (getvar "osmode"))
	(setvar "clayer" FLLT_AXNM)
	(setvar "osmode" 0)

	(setq 
		c_ss (ssget '((0 . "CIRCLE")))
		c_amt (if c_ss (sslength c_ss) 0)
		c_idx 0
		ss_del (ssadd)
	)
	(repeat c_amt
		(setq 
			c_en 	(ssname c_ss c_idx)
			c_ent 	(entget c_en)
			c_cen 	(trans (cdr (assoc 10 c_ent)) 0 1)
			c_rad 	(cdr (assoc 40 c_ent))
			scl 	(* 0.25 c_rad)
			pt1 	(polar c_cen (* 1.25 pi) c_rad)
			pt2 	(polar c_cen (* 0.25 pi) c_rad)
			t_ss 	(ssget "c" pt1 pt2)
			t_ss 	(ssget "p" '((0 . "TEXT")))
		)
		(if t_ss
			(progn
				(setq 
					t_en (ssname t_ss 0)
					att (strcase (cdr (assoc 1 (entget t_en))))
					axi (if (> (strlen att) 1) "axi" "axi0")
				)
				(cmd "insert" axi c_cen scl "" "0" att)
				(setq 
					ss_del (ssadd c_en ss_del)
					ss_del (ssadd t_en ss_del)
				)
			)
		)
		(setq c_idx (1+ c_idx))
	)
	(if ss_del (cmd "erase" ss_del ""))

	(setvar "clayer" cl)
	(setvar "osmode" os)
	(setvar "cmdecho" 1)

	(princ)
)

(princ "loaded. Start as C:CVA ")
(princ)
