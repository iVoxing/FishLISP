; v1.0 1998-06-10 origenal version
; off/freeze layers by selecting objects; and restore them at one time
; v2.0 2000-07-04 no freeze anymore, new ADD option to add selection set
; rename to SF
;

(defun c:sf (/ ss amt lay_list idx lay_name key lay_all str)
	(prompt "\nFishLISP, 6-10-1998. Ver 2.0")
	(setq olderr *error*)
	(defun *error* (s)
		(setq ss nil amt nil lay_list nil idx nil lay_name nil key nil lay_all nil str nil)
		(setvar "cmdecho" 0)
		(fl_undo_end)
		(setvar "cmdecho" 1)
		(setq *error* olderr olderr nil)
		(princ)
	)

	(setvar "cmdecho" 0)
	(fl_undo_begin)

	(if snf_lay
		(progn
			(princ (strcat "\nLayer(s) stored: " snf_lay ".\n[Restore/Add/Delete]<Select object(s)>: "))
			(initget "Restore Add Delete")
			(setq key (getkword))
			(cond
				((= key "Restore") (setq key "on"))
				((= key "Delete") t)
				((= key "Add") (setq key "off" snf_add t))
				(t (setq key "off"))
			)
		)
		(setq key "off")
	)
	(if (= key "Delete")
		(progn
			(cmd "layer" "on" snf_lay "")
			(setq ss_del (ssget "x" (list (cons 8 snf_lay))))
			(cmd "erase" ss_del "")
			(setq snf_lay nil)
		)
	)

	(if (= key "off")
		(setq 
			ss (ssget)
			lay_list (list)
			idx 0
		)
	)
	(setq amt (if ss (sslength ss) 0))

	(repeat amt
		(setq lay_name (cdr (assoc 8 (entget (ssname ss idx)))))
		(if (member lay_name lay_list)
			nil
			(if (= lay_name (getvar "clayer"))
				nil
				(setq lay_list (append lay_list (list lay_name)))
			)
		)
		(setq idx (1+ idx))
	)

	(if lay_list
		(setq lay_all (substr (apply 'strcat (mapcar '(lambda (str) (strcat "," str)) lay_list)) 2))
		(if (= key "on") (setq lay_all snf_lay))
	)
	(if lay_all
		(progn
			(if snf_add (setq lay_all (strcat snf_lay "," lay_all)))
			(setvar "cmdecho" 0)
			(cmd "layer" key lay_all "")
			(setq snf_lay (if (= key "off") lay_all nil))
			(setq snf_add nil)
		)
	)

	(setvar "cmdecho" 0)
	(fl_undo_end)
	(setvar "cmdecho" 1)
	(setq *error* olderr)
	(princ)
)

(princ "loaded. Start as C:SF. ")
(princ)
