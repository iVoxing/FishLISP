; FishLISP C:DF
; �������Ⱦ�OFFSET
; 2004-07-05v1.0 ��ʽ�档���ӱ��桢�ָ�ԭ�е�OFFSETDISTϵͳ������
; 2002-06-20v0.9 ���ð档

(defun c:df (/ df_str df_list idx dis en0 et0 pt0 pt1 of_dist)
	(princ "\nOffset distance:<")
	(if df_defaut (princ df_defaut))
	(princ "> ")
	(setq df_str (getstring 1))
	(if (= df_str "")
		(setq df_list df_defaut)
		(setq df_list (read (strcat "(" df_str ")")))
	)
	(setq of_dist (getvar "offsetdist"))
	(setvar "cmdecho" 0)
	(if df_list
		(progn
			(setq idx 0)
			(setq dis 0)
			(setq en0 (entsel "\nSelect object: "))
			(setq et0 (car en0))
			(setq pt0 (cadr en0))
			(setq pt1 (getpoint "\nSpecify point on side to offset: "))
			(repeat (length df_list)
				(setq dis (+ dis (nth idx df_list)))
				(cmd "offset" dis et0 pt1 "")
				(setq idx (1+ idx))
			);repeat
			(setq df_defaut df_list)
		);if df_list true
	);if df_list
	(setvar "cmdecho" 1)
	(setvar "offsetdist" of_dist)
	(princ)
)

(princ "FishLISP C:DF. ")
(princ)
