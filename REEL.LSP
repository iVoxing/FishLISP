;	2014-04-24	增加圖紙單位為米的處理

(defun C:REEL (/ ss n en att elent atent unit_scale)
	(princ "\nFishLISP, Nov 19 1995")
	(princ "\nChange attribute of EL-blocks, ver 2.0. Start as C:REEL")
	(setq 
		ss (ssget '((0 . "insert") (2 . "elur,elul,eldr,eldl,dim_bg")))
		n 0
	)
	(setq unit_scale (if (= (getvar "insunits") 6) 1.0 1000.0))
	(if ss
		(repeat (sslength ss)
			(setq 
				en (ssname ss n)
				elent (entget en)
				atent (entget (entnext en))
				att (rtos (/ (- (caddr (assoc 10 elent)) el_h) unit_scale) 2 3)
				att (if (= att "0.000") "%%P0.000" att)
			)
			(entmod (subst (cons 1 att) (assoc 1 atent) atent))		;modify attr
			(entupd en)						;update block
			(setq n (1+ n)) 						;go to next one
		)
	)
	(princ)
)



(defun C:ELADD (/ ss n en att attadd attv oldvlu newvlu)
	(princ "\nFishLISP, Nov 19 1995")
	(princ "\nChange attribute of EL-blocks, ver 3.0. Start as C:ELADD")
	; V3.0 2000-3-3, Default value function added.
	(if g:readd nil (setq g:readd 0.0))
	(princ "\nDelta value: <")
	(princ g:readd)
	(setq attadd (getreal "> "))
	(if attadd nil (setq attadd g:readd))
	(setq 
		ss (ssget '((0 . "insert") (2 . "elur,elul,eldr,eldl,al,ar,bg,dim_bg,dim_rl")))
		n 0
		g:readd attadd
	)
	(if ss
		(repeat (sslength ss)
			(setq 
				en (ssname ss n)
				att (entget (entnext en))
				oldvlu (cdr (assoc 1 att))
			)
			(if (= oldvlu "%%P0.000")
				(setq oldvlu "0.000")
			)
			(setq 
				attv (+ (distof oldvlu) attadd)
				newvlu (rtos attv 2 3)
			)
			(if (= newvlu "0.000") (setq newvlu "%%P0.000"))
			(entmod (subst (cons 1 newvlu) (assoc 1 att) att))		;modify atte
			(entupd en) 						;update block
			(setq n (1+ n))					;go to next one
		)
	)
	(princ)
)


(princ "loaded. Start as C:REEL, C:ELADD.")
(princ)
