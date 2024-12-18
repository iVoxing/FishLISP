; 1/16/98	v4.0	Directly set the scale in modelspace;
;

(defun c:scset (/ set_sc temperr olderr vpt vptent vpt_h vpt_id vw_h psc sc_z)
	(princ "\nFishLISP. Ver 4.0")
	(defun set_sc (/ vw_h)
		(setq
			vw_h (getvar "viewsize")
			psc (/ vw_h vpt_h)
		)
		(cond
			((<= psc 50) (setq psc 50))
			((and (> psc 50.0) (<= psc 75.0)) (setq psc 75))
			((and (> psc 75.0) (<= psc 100.0)) (setq psc 100))
			((and (> psc 100.0) (<= psc 120.0)) (setq psc 120))
			((and (> psc 120.0) (<= psc 150.0)) (setq psc 150))
			(t)
		);cond
		(princ "\nNew scale: 1/<")
		(princ psc)
		(initget 128)
		(setq sc_z (getreal ">XP: "))
		(if (numberp sc_z) nil (setq sc_z psc))
		(setq sc_z (strcat (rtos (/ 1.0 sc_z) 2 20) "xp"))
		(cmd "_zoom" sc_z)
	)

	(setq olderr *error*)
	(defun *error* (s)
		(setvar "regenmode" 1)
		(setvar "cmdecho" 1)
		;(setq *error* olderr olderr nil temperr nil)
		;(setq set_sc nil vpt nil vpten nil vpt_h nil vpt_id nil vw_h nil psc nil sc_z nil)
		(princ)
	)

	(setvar "cmdecho" 0)
	(setvar "tilemode" 0)
	(setvar "regenmode" 0)
	(if (= (getvar "cvport") 1)
		(progn
			(cmd "_layer" "t" "vp" "")
			(setq vpt (entsel "\nSelect a viewport: "))
			(if (and vpt (= (cdr (assoc 0 (setq vptent (entget (car vpt))))) "VIEWPORT"))
				(progn
					(setq 
						vpt_h (cdr (assoc 41 vptent))
						vpt_id (cdr (assoc 69 vptent))
					)
					(cmd "_mspace")
					(setvar"cvport" vpt_id)
					(set_sc)
					(cmd "_pspace")
				)
				(if vpt (princ "\n1 was not viewport. "))
			)
		)
		(setq 
			vptent (entget (ssname (ssget "x" (list '(0 . "viewport") (cons 69 (getvar "cvport")))) 0))
			vpt_h (cdr (assoc 41 vptent))
			temp (set_sc) 
			temp nil
		)
	)
	(setvar "regenmode" 1)
	(setvar "cmdecho" 1)
	(setq *error* olderr olderr nil)
	(princ)
)

	; 1/15/98	v3.0	Judge the selection is dimension text or not while selecting.
	;

(defun c:dsc (/ olderr en loop en0 ent hi)
	(princ "\nFishLISP. Version 3.0")
	(setq olderr *error*)
	(defun *error* (s)
		(setq *error* olderr olderr nil en nil loop nil en0 nil ent nil hi nil)
		(princ)
	)

	(setq loop t)
	(while loop
		(setq en (nentsel "\nSelect a dimension text: "))
		(if
			(and 
				en 
				(= (type (setq en0 (car (last en)))) 'ENAME)
				(= (cdr (assoc 0 (setq ent (entget (car en))))) "MTEXT")
				(= (cdr (assoc 0 (entget en0))) "DIMENSION")
			)
			(setq loop nil hi (/ (cdr (assoc 40 ent)) 3.0))
			(if en (princ "\n1 was not dimension text. ") (setq loop nil))
		)
		(if hi (princ (strcat "\nThe dimension scale is 1/" (rtos hi 2 1))))
	)

	(setq *error* olderr)
	(princ)
)

(defun c:dg (/ olderr en loop en0 ent hi sc)
	(princ "\nFishLISP. Version 1.0")
	(setq olderr *error*)
	(defun *error* (s)
		(setq *error* olderr olderr nil en nil loop nil en0 nil ent nil hi nil)
		(princ)
	)
	(setq loop t)
	(while loop
		(setq en (nentsel "\nSelect a dimension text: "))
		(if
			(and 
				en 
				(= (type (setq en0 (car (last en)))) 'ENAME)
				(= (cdr (assoc 0 (setq ent (entget (car en))))) "MTEXT")
				(= (cdr (assoc 0 (entget en0))) "DIMENSION")
			)
			(setq loop nil hi (/ (cdr (assoc 40 ent)) 3.0))
			(if en (princ "\n1 was not dimension text. ") (setq loop nil))
		)
		(if hi
			(progn
				(princ (strcat "\nDimension scale: <" (rtos hi 2 1) "> "))
				(setq sc (getreal))
				(setq sc (if sc sc hi))
				(setvar "dimscale" sc)
			)
		)
	)

	(setq *error* olderr)
	(princ)
)


(princ "loaded.\nFishLISP. Scale setting utilities. Start as C:SCSET C:DSC ")
(princ)
