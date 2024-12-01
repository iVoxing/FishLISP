; Ìí¼ÓÖáÏßºÅ
(load "function.lsp")

(fl_layer_check "PAXIS" "134")
(fl_layer_check "PAXINUM" "7")

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
			);progn
			(setq loop nil)
		);if en
	);while
	(princ)
)

(defun ins_aid (ent pt0 id_str / ln1 ln2 sym sc os cl attr ang ins)
	(setq
		pt0 (trans pt0 1 0)
		pt1 (cdr (assoc 10 ent))
		pt2 (cdr (assoc 11 ent))
		ln1 (distance pt0 pt1)
		ln2 (distance pt0 pt2)
		id_str (strcase id_str)
		sym (if (> (strlen id_str) 1) "axi" "axi0")
		sc (getvar "dimscale")
		os (getvar "osmode")
		cl (getvar "clayer")
		attr (getvar "attreq")
	)
	(if (> ln1 ln2)
		(setq tmp pt1 pt1 pt2 pt2 tmp tmp nil)
	)
	(setq 
		ang (angle pt2 pt1)
		ins (polar pt1 ang (* sc 4))
		ins (trans ins 0 1)
	)
	(setvar "attreq" 1)
	(setvar "attdia" 0)
	(setvar "blipmode" 0)
	(setvar "osmode" 0)
	(setvar "clayer" "paxinum")
	(setvar "cmdecho" 0)
	(cmd "insert" sym ins (if (= (getvar "insunits") 6) (* 1000 sc) sc) "" 0 (strcase id_str))
	(setvar "attreq" attr)
	(setvar "blipmode" 1)
	(setvar "osmode" os)
	(setvar "clayer" cl)
)

(defun cvtlin (/ kwd)
	(if (= en_lay "PAXIS")
		nil
		(progn
			(initget "Yes No")
			(setq kwd (getkword "\nConvert line to Axis? [<Yes>/No] "))
			(if kwd nil (setq kwd "Yes"))
			(if (= kwd "Yes")
				(entmod (subst (cons 8 "PAXIS") (assoc 8 ent) ent))
				(setq loop nil)
			)
		)
	)
	loop
)

(princ "loaded. Start as C:AID. ")
(princ)
