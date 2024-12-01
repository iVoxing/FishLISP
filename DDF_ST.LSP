; 1/8/98	v2.0	Dialogue ready.
; 1/8/98	v2.1	Better drawing on.
; 1/9/98	v3.0	Logo added.

(defun c:ddst (/ init_option calc accept_option drw_line drw_st stp_h stp_w stp_n tmp)
	(defun init_option ()
		(if f_str_h (set_tile "st_h_box" (rtos f_str_h)))
		(if f_str_w (set_tile "st_w_box" (rtos f_str_w)))
		(set_tile "sp_h_box" (if f_stp_h (rtos f_stp_h) "150"))
		(if f_stp_w (set_tile "sp_w_box" (rtos f_stp_w)))
		
		(if (= f_st_dir "Right") (set_tile "left_rad" "1"))
		(if (= f_st_dir "Left") (set_tile "right_rad" "1"))
		(if (= f_st_seg "Section") (set_tile "ele_rad" "1"))
		(if (= f_st_seg "Elevation") (set_tile "sec_rad" "1"))

		(setq 
			stp_n (fix (/ (atof (get_tile "st_h_box")) (atof (get_tile "sp_h_box"))))
			str_l (* (atof (get_tile "sp_w_box")) (1- stp_n))
		)
		(set_tile "amt_txt" (strcat "Amount: " (rtos stp_n)))
		(set_tile "len_txt" (strcat "Length:" (rtos str_l)))

		(start_image "logo")
		(slide_image 1 1 136 64 "logo")
		(end_image)

		(mode_tile "len_txt" 1)
		(mode_tile "amt_txt" 1)
		(mode_tile "st_w_box" 1)
		(mode_tile "sp_w_box" 1)
	)

	(defun calc (/ str_h str_l stp_h stp_w stp_n)
		(setq 
			str_h (atof (get_tile "st_h_box"))
			stp_h (atof (get_tile "sp_h_box"))
			stp_h (if (> stp_h 175) 175 stp_h)
			stp_n (fix (+ 1 (/ str_h stp_h)))
			stp_h (/ str_h stp_n)
			stp_w (fix (- 450.0 stp_h))
			stp_w (if (< stp_w 260) 260 stp_w)
			str_l (* stp_w (1- stp_n))
		)
		(set_tile "sp_h_box" (rtos stp_h))
		(set_tile "sp_w_box" (rtos stp_w))
		(set_tile "len_txt" (strcat "Length:" (rtos str_l)))
		(set_tile "amt_txt" (strcat "Amount: " (rtos stp_n)))
		(mode_tile "len_txt" 0)
		(mode_tile "amt_txt" 0)
		(mode_tile "sp_w_box" 0)
	)

	(defun accept_option ()
		(setq 
			stp_h (atof (get_tile "sp_h_box"))
			stp_w (atof (get_tile "sp_w_box"))
			stp_n (atoi (substr (get_tile "amt_txt") 11))
			f_str_h (atof (get_tile "st_h_box"))
			f_str_w (atof (get_tile "st_w_box"))
			f_stp_h stp_h
			f_stp_w stp_w
		)
		(if (= (get_tile "left_rad") "1") (setq f_st_dir "Left" dir pi))
		(if (= (get_tile "right_rad") "1") (setq f_st_dir "Right" dir 0))
		(if (= (get_tile "sec_rad") "1") (setq f_st_seg "Section"))
		(if (= (get_tile "ele_rad") "1") (setq f_st_seg "Elevation"))
		(done_dialog 1)
	)

	(defun drw_line (lpt1 lpt2 / lay)
		(if (= f_st_seg "Section") (setq lay FLLT_WALL) (setq lay "0"))
		(entmake (list '(0 . "line") (cons 8 lay) (cons 10 (trans lpt1 1 0)) (cons 11 (trans lpt2 1 0))))
	)

	(defun drw_st (/ a90 pt0 pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 pt9 pt10)
		(setvar "cmdecho" 0)
		(cmd "zoom" "1x")
		(setq 
			pt0 (getpoint "\nStart point of stair: ")
			pt1 pt0
			a90 (/ pi 2)
		)
		(if (and f_st_del (equal pt0 (cdr (assoc 11 (entget f_st_del)))))
			(progn
				(entdel f_st_del)
				(setq f_st_del nil)
			)
		)
		(repeat (1- stp_n)
			(setq 
				pt2 (polar pt1 a90 stp_h)
				pt3 (polar pt2 dir stp_w)
			)
			(if pt4 nil (setq pt4 (polar pt2 a90 -300)))
			(drw_line pt1 pt2)
			(drw_line pt2 pt3)
			(setq pt1 pt3)
		)
		(setq 
			pt2 (polar pt1 a90 stp_h)
			pt3 (polar pt2 dir stp_w)
		)
		(if pt4 nil (setq pt4 (polar pt2 a90 -300)))
		(drw_line pt1 pt2)
		(if (= f_st_seg "Section") nil
			(setq f_st_del (entlast))
		)
		(setq 
			pt5 (polar pt0 a90 -300)
			pt6 (polar pt5 dir -200)
			pt7 (polar pt6 a90 200)
			pt8 (polar pt2 a90 -300)
			pt9 (polar pt8 dir 200)
			pt10 (polar pt9 a90 200)
		)
		(if (= f_st_seg "Section") 
			(progn
				(drw_line pt4 pt5)
				(drw_line pt5 pt6)
				(drw_line pt6 pt7)
				(drw_line pt8 pt9)
				(drw_line pt9 pt10)
			)
		)
		(drw_line pt4 pt8)
		(setvar "cmdecho" 1)
	)

; =========== main =============================
	(princ "\nFishLISP. Version 2.1.")
	(setq dcl_id (load_dialog "ddf_st"))
	(new_dialog "f_stair" dcl_id)
	(init_option)
	(action_tile "cal_btn" "(calc)")
	(action_tile "accept" "(accept_option)")
	(if (= (start_dialog) 1) (drw_st))
	(princ)
)

; ============ Loading infomation ===================

(princ "Start as C:DDST")
(princ)