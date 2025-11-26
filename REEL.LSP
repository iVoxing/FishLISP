;	2014-04-24	增加圖紙單位為米的處理
;	2000-03-03	Default value function added.
;	1995-11-19

(defun C:REEL (/ ss n en att elent atent unit_scale)
	(princ "\nChange attribute of EL-blocks. C:REEL")
	(setq 
		ss (ssget '((0 . "INSERT") (2 . "ELUR,ELUL,ELDR,ELDL,DIM_BG")))
		n 0
	)
	(setq unit_scale (if (= (getvar "insunits") 6) 1.0 1000.0))
	(if ss
		(repeat (sslength ss)
			(setq 
				en (ssname ss n)
				elent (entget en)
				atent (entget (entnext en))
				att (rtos (/ (- (caddr (assoc 10 elent)) EL_BASE_Y) unit_scale) 2 3)
				att (if (= att "0.000") "%%P0.000" att)
			)
			(entmod (subst (cons 1 att) (assoc 1 atent) atent))
			(entupd en)
			(setq n (1+ n))
		)
	)
	(princ)
)



(defun C:ELADD (/ ss n en att attadd attv oldvlu newvlu)
	(princ "\nChange attribute of EL-blocks.")
	(setq EL_DELTA (if EL_DELTA EL_DELTA 0.0))
	(setq attadd (getreal (strcat "\nDelta value: <" (rtos EL_DELTA) "> ")))
	(setq attadd (if attadd attadd EL_DELTA))
	(setq EL_DELTA attadd)
	(setq 
		ss (ssget '((0 . "INSERT") (2 . "ELUR,ELUL,ELDR,ELDL,AL,AR,BG,DIM_BG,DIM_RL")))
		n 0
	)
	(if ss
		(repeat (sslength ss)
			(setq 
				en (ssname ss n)
				att (entget (entnext en))
				oldvlu (cdr (assoc 1 att))
				attv (+ (atof oldvlu) attadd)
				newvlu (rtos attv 2 3)
			)
			(if (= newvlu "0.000") (setq newvlu "%%P0.000"))
			(entmod (subst (cons 1 newvlu) (assoc 1 att) att))
			(entupd en)
			(setq n (1+ n))
		)
	)
	(princ)
)

(princ "loaded. Start as C:REEL, C:ELADD.")
(princ)
