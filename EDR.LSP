; 删除Door，并且恢复墙线。
;	2023-10-06	l58 开始，好像可以重写


(defun c:edr ();/ olderr en ent pt0 ang wid ptm1 ptm2 ss l1 lent1 pt11 pt12 pt21 pt22 l11 l12 l21 l22 lent11 lent12 lent 21 lent22 pt111 pt112 pt211 pt212 pt121 pt122 pt221 pt222 npt11 npt21 npt12 npt22 idx wid1 ang1)
	(setq olderr *error*)
	(defun *error* (s)
		(setvar "cmdecho" 0)
		(fl_undo_end)
		(setq *error* olderr olderr nil)
		(princ)
	)
	(setvar "cmdecho" 0)
	(fl_undo_begin)
	(setq en (entsel "\nSelect a door to earse: "))
	(if
		(and
			en
			(= "INSERT" (cdr (assoc 0 (setq ent (entget (car en))))))
			(member (strcase (cdr (assoc 2 ent))) (list "DR1" "DR2" "DR3" "DR4" "DR5" "DR6" "DR7" "DR8"))
		)
		(progn
			(setq 
				pt0 (cdr (assoc 10 ent))
				ang (cdr (assoc 50 ent))
				wid (abs (/ (cdr (assoc 41 ent)) 0.002))
			)
			(entdel (car en))
			(setq 
				ptm1 (polar pt0 ang wid)
				ptm2 (polar pt0 ang (- wid))
			)
			(setq ss (ssget "f" (list ptm1 ptm2) (list (cons 8 FLLT_WALL))))
			(cond
				((not ss)
					(princ "\n1 DOOR was not in the Wall. ")
				)
				( t
					(setq 
						l1 (ssname ss 0)
						lent1 (entget l1)
						pt11 (cdr (assoc 10 lent1))
						pt12 (cdr (assoc 11 lent1))
						wid1 (/ (distance pt11 pt12) 2.0)
						ang1 (angle pt11 pt12)
						pt11 (polar ptm1 ang1 wid1)
						pt12 (polar ptm1 ang1 (- wid1))
						pt21 (polar ptm2 ang1 wid1)
						pt22 (polar ptm2 ang1 (- wid1))
					)
					(if (inters pt11 pt21 pt12 pt22)
						(setq temp pt12 pt12 pt22 pt22 temp temp nil)
					)
					(setq idx 0)
					(repeat (sslength ss)
						(entdel (ssname ss idx))
						(setq idx (1+ idx))
					)
					(setq 
						l11 (ssname (ssget "x" (list (cons 8 FLLT_WALL) '(-4 . "<or") (cons 10 pt11) (cons 11 pt11) '(-4 . "or>"))) 0)
						l12 (ssname (ssget "x" (list (cons 8 FLLT_WALL) '(-4 . "<or") (cons 10 pt12) (cons 11 pt12) '(-4 . "or>"))) 0)
						l21 (ssname (ssget "x" (list (cons 8 FLLT_WALL) '(-4 . "<or") (cons 10 pt21) (cons 11 pt21) '(-4 . "or>"))) 0)
						l22 (ssname (ssget "x" (list (cons 8 FLLT_WALL) '(-4 . "<or") (cons 10 pt22) (cons 11 pt22) '(-4 . "or>"))) 0)
						lent11 (entget l11)
						lent12 (entget l12)
						lent21 (entget l21)
						lent22 (entget l22)
						pt111 (cdr (assoc 10 lent11))
						pt112 (cdr (assoc 11 lent11))
						npt11 (if (< (distance pt11 pt111) (distance pt11 pt112)) pt112 pt111)
						pt211 (cdr (assoc 10 lent21))
						pt212 (cdr (assoc 11 lent21))
						npt21 (if (< (distance pt21 pt211) (distance pt21 pt212)) pt212 pt211)
						pt121 (cdr (assoc 10 lent12))
						pt122 (cdr (assoc 11 lent12))
						npt12 (if (< (distance pt12 pt121) (distance pt12 pt122)) pt122 pt121)
						pt221 (cdr (assoc 10 lent22))
						pt222 (cdr (assoc 11 lent22))
						npt22 (if (< (distance pt22 pt221) (distance pt22 pt222)) pt222 pt221)
					)
					(entdel l11)
					(entdel l12)
					(entdel l21)
					(entdel l22)
					(entmake (list '(0 . "line") (cons 8 FLLT_WALL) (cons 10 npt11) (cons 11 npt21)))
					(entmake (list '(0 . "line") (cons 8 FLLT_WALL) (cons 10 npt12) (cons 11 npt22)))
				); cap check passed end
			); end of conditon
		);progn
		(cond
			((not en))
			(t (princ "\n1 was not DOOR. "))
		)
	);if and ...
	(fl_undo_end)
	(setq *error* olderr olderr nil)
	(princ)
)

(princ "loaded. Start as C:EDR ")
(princ)
