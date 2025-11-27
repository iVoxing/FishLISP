; ²åÈëÖáÏßºÅ
; 	2025-11-12	rewrite variables get|set, tested ok
;				
(load "function.lsp")

(fl_layer_check FLLT_AXIS "134")

(defun c:aid (/ loop en en_typ en_lay pickpt str msg)
	(setq loop t)
	(while loop
		(setq en (entsel "\nSelect an AXIS line: "))
		(if en
			(progn
				(setq
					en_typ (cdr (assoc 0 (setq ent (entget (car en)))))
					en_lay (strcase (cdr (assoc 8 ent)))
					pickpt (cadr en)
				)
				(if (= en_typ "LINE")
					(if (cvtlin)
						(if (setq str (getstring "\nAxis ID: "))
							(ins_aid ent pickpt str)
							(setq loop nil)
						)
						(setq loop nil msg (princ "\n1 was not Axis-line. "))
		  			)
					(setq loop nil msg (princ "\n1 was not Line. "))
				)
	  		)
			(setq loop nil)
		)
  	)
	(princ)
)

(defun ins_aid (ent_ pt0_ id_str_ / ln1 ln2 sym sc os cl attr ang ins)
	(setq
		pt0_ (trans pt0_ 1 0)
		pt1 (cdr (assoc 10 ent_))
		pt2 (cdr (assoc 11 ent_))
		ln1 (distance pt0_ pt1)
		ln2 (distance pt0_ pt2)
		id_str_ (strcase id_str_)
		sym (if (> (strlen id_str_) 1) "axi" "axi0")
        sc (getvar "dimscale")
		var_list (list "osmode" "clayer" "attreq")
		var_saved (mapcar 'getvar var_list)
  	)
  
	(if (> ln1 ln2)
		(setq tmp pt1 pt1 pt2 pt2 tmp tmp nil)
 	)

	(setq
		ang (angle pt2 pt1)
		ins (polar pt1 ang (* sc 4))
		ins (trans ins 0 1)
  	)

   	(mapcar 'setvar
		(list "attreq"	"attdia"	"blipmode"	"osmode"	"cmdecho"	"clayer")
		(list 1			0			0			0			0			FLLT_AXIS)
  	)
	(cmd "_insert" sym ins (if (= (getvar "insunits") 6) (* 1000 sc) sc) "" 0 (strcase id_str_))
  	(mapcar 'setvar var_list var_saved)
	(setvar "blipmode" 1)
)

(defun cvtlin (/ kwd)
	(if (= en_lay FLLT_AXIS)
		nil
		(progn
			(initget "Yes No")
			(setq kwd (getkword "\nConvert line to Axis? [<Yes>/No] "))
			(setq kwd (if kwd kwd "Yes"))
			(if (= kwd "Yes")
				(entmod (subst (cons 8 FLLT_AXIS) (assoc 8 ent) ent))
				(setq loop nil)
	  		)
		)
  	)
	loop
)

(princ "loaded. Start as C:AID. ")
(princ)
