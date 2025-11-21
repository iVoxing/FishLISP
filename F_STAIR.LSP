; 2025-11-21		rewriting
; 1998-01-08 v2.0	Drawing module added.

(defun c:fst (/ str_h str_l str_w stp_h stp_w stp_n )
	(princ "\FishLISP. Draw stair section. Version 2.0 ")

	(setq 
		STAIR_HEIGHT (if STAIR_HEIGHT STAIR_HEIGHT 3000.0)
		STEP_HEIGHT (if STEP_HEIGHT STEP_HEIGHT 150.0)
		str_h (getreal (strcat "\nStair height: <" STAIR_HEIGHT ">"))
		str_h (if str_h str_h STAIR_HEIGHT)
		STAIR_HEIGHT str_h

		stp_h (getreal (strcat "\nStep height: <" STEP_HEIGHT ">"))
		stp_h (if stp_h stp_h STEP_HEIGHT)
		STEP_HEIGHT stp_h

		stp_n (fix (/ str_h stp_h))
		stp_h (/ str_h stp_n)
		stp_w (fix (- 450.0 stp_h))
		STEP_WIDTH stp_w
		
		stp_w (getreal (strcat "\nStep width: <" STEP_WIDTH ">:"))
		stp_w (if stp_w stp_w STEP_WIDTH)
		STEP_WIDTH stp_w

		str_l (* (1- stp_n) stp_w)
	)
	
	(princ
		(strcat	
			"\nSTAIR == Hight: "
			(rtos str_h 2 0)
			"\tLength: "
			(rtos str_l 2 0)
			"\nSTEPS == Hight: "
			(rtos stp_h 2 0)
			"\tWidth:"
			(rtos stp_w 2 0)
			"\tAmount: "
			(rtos stp_n 2 0)
		)
	)
	(initget "Yes No")
	(setq kwd (getkword "\nDraw stair? <Yes>/No"))
	(setq kwd (if kwd kwd "Yes"))
	(if (= kwd "Yes") (drw_st))
	(princ)
)

(defun drw_st (/ a90 pt0 pt1 pt2 pt3 pt4 pt5 pt6 pt7 pt8 pt9 pt10)
	(setvar "cmdecho" 0)
	(cmd "zoom" "1x")
	(setq 
		pt0 (getpoint "\nStart point of stair: ")
		pt1 pt0
		a90 (/ pi 2)
	)
	(initget "Left Right")
	(cond 
		((= f_st_dir "Left")
			(setq kwd (getkword "\nchoice direction: [Left]<Right> "))
			(setq kwd (if kwd kwd "Right"))
		)
		((= f_st_dir "Right")
			(setq kwd (getkword "\nChoice direction: [Right]<Left> "))
			(setq kwd (if kwd kwd "Left"))
		)
		((not f_st_dir)
			(setq kwd (getkword "\nchoice direction: [Left/Right] "))
		)
	)
	(setq f_st_dir kwd)
	(if (= kwd "Left") (setq dir pi) (setq dir 0))

	(initget "Section Elevation")
	(princ "\nStair segment is: ")
	(cond 
		((= f_st_seg "Section")
			(setq kwd (getkword "[Section]<Elevation> "))
			(setq kwd (if kwd kwd "Elevation"))
		)
		((= f_st_seg "Elevation")
			(setq kwd (getkword "[Elevation]<Section> "))
			(setq kwd (if kwd kwd "Section"))
		)
		((not f_st_seg)
			(setq kwd (getkword "[Elevation/Section] "))
		)
	)
	(setq f_st_seg kwd)
	(repeat stp_n
		(setq 
			pt2 (polar pt1 a90 stp_h)
			pt3 (polar pt2 dir stp_w)
		)
		(setq pt4 (if pt4 pt4 (polar pt2 a90 -300)))
		(drw_line pt1 pt2)
		(drw_line pt2 pt3)
		(setq pt1 pt3)
	)
	(entdel (entlast))
	(if (= kwd "Elevation") (entdel (entlast)))
	(setq 
		pt5 (polar pt0 a90 -300)
		pt6 (polar pt5 dir -200)
		pt7 (polar pt6 a90 200)
		pt8 (polar pt2 a90 -300)
		pt9 (polar pt8 dir 200)
		pt10 (polar pt9 a90 200)
	)
	(if (= kwd "Section") 
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


(defun drw_line (lpt1_ lpt2_ / lay)
	(if (= kwd "Section") (setq lay FLLT_WALL) (setq lay "0"))
	(entmake (list '(0 . "line") (cons 8 lay) (cons 10 (trans lpt1_ 1 0)) (cons 11 (trans lpt2_ 1 0))))
)

(princ "Start as C:FST")
