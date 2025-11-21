; 2025-11-16	rewrite, test ok

; return: '('(var_name var_value) ...)
(defun var_save (var_name_list_)
	(mapcar 
		'(lambda (var_name_) (list var_name_ (getvar var_name_)))
		var_name_list_
	)
)

(defun var_restore (var_list_)
	(mapcar 
		'(lambda (var_)
			(setvar (car var_) (cadr var_))
		)	
		var_list_
	)
)

; varlist: '('(var_name var_value)...)
; use list or dict? wich better?
(defun var_set (var_list_)
	(if FL:SAVE_VARS
		(setq FL:VARS
			(mapcar
				'(lambda (var_)
					(list (car var_) (getvar (car var_)))
				)
				var_list_
			)
		)
	)
	(mapcar
		'(lambda (var_)
			(setvar (car var_) (cadr var_))
		)
		var_list_
	)
)

(defun c:test ()
	(setq FL:SAVE_VARS t)
	(setq vlist (var_save '("osmode" "cmdecho" "clayer" "plinewid")))
	(princ "\nTest var_save: ")
	(princ vlist)
	(setq new_list (var_set (list '("osmode" 0) '("cmdecho" 0) '("clayer" "0") '("plinewid" 0))))
	(princ "\nTest FL:VARS: ")
	(princ FL:VARS)
	(princ "\nTest New value: ")
	(princ new_list)
	(getstring "\nPuased. <Continue>")
	(princ "\nTest var_restore: ")
	(princ (var_restore vlist))
	(princ)
)