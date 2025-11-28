; FishLISP C:DF
; 连续不等距OFFSET
; 2025-11-28		重构
; 2004-07-05 v1.0 	正式版。增加保存、恢复原有的OFFSETDIST系统变量。
; 2002-06-20 v0.9 	可用版。

(defun c:df (/ df_str df_list idx dis en0 et0 pt0 pt1 of_dist)
	(princ "\nOffset distance:<")
	(if df_default (princ df_default))
	(princ "> ")
	(setq df_str (getstring 1))
	(if (= df_str "")
		(setq df_list df_default)
		(setq df_list (read (strcat "(" df_str ")")))
	)
	(setq of_dist (getvar "offsetdist"))
	(setvar "cmdecho" 0)
	(if df_list
		(setq
			df_default df_list
			idx 0
			dis 0
			en0 (entsel "\nSelect object: ")
		)
	)
	(if en0
		(setq 
			et0 (car en0)
			pt0 (cadr en0)
			pt1 (getpoint "\nSpecify point on side to offset: ")
		)
	)
	(if pt1
		(repeat (length df_list)
			(setq dis (+ dis (nth idx df_list)))
			(cmd "_offset" dis et0 pt1 "")
			(setq idx (1+ idx))
		)
	)
	(setvar "cmdecho" 1)
	(setvar "offsetdist" of_dist)
	(princ)
)

(princ "FishLISP C:DF. ")
(princ)
