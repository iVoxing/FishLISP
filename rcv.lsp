(defun c:rcv (/ os cmd)
	(setq os (getvar "osmode")
		cmd (getvar "cmdecho")
	)
	(setvar "osmode" 0)
	(setvar "cmdecho" 0)
	(fl_make_modi_layer)
	(command "_rectang" pause pause)
	(command "_revcloud" "o" "l" "")
	(setvar "osmode" os)
	(setvar "cmdecho" cmd)
	(princ)
)