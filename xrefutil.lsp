; FishLISP XREF utilities
; XREF fast operation
; C:XRR
; C:XRU
; C:XRD
; C:XRBB
; C:XRBI
; -----------------------------------------------
; 2004-07-12 v1.0
; -----------------------------------------------


(defun get_xref_name (/ en ent ety ben xname)
	(if (setq en (entsel "\n选取XREF图块："))
		(progn
			(setq
				ent (entget (car en))
				ety (cdr (assoc 0 ent))
			)
			(if (= ety "INSERT")
				(progn
					(setq ben (tblsearch "block" (cdr (assoc 2 ent))))
						; (setq aaa ben)
					(if (= 4 (logand 4 (cdr (assoc 70 ben))))
						(setq xname (cdr (assoc 2 ben)))
						(princ "\n对象不是XREF图块。")
					)
				)
				(princ "\n对象不是图块。")
			)
		)
	)
	xname ; retuen value
)

(defun xref_cmd (cmdname)
	(setvar "cmdecho" 0)
	(cmd "xref" cmdname)
	(setvar "cmdecho" 1)
	cmd (get_xref_name))
)

(defun c:xrr () (xref_cmd "reload") (princ))

(defun c:xrd () (xref_cmd "detach") (princ))

(defun c:xru () (xref_cmd "unload") (princ))

(defun c:xrbb (/ bity)
	(setq bity (getvar "bindtype"))
	(setvar "bindtype" 0)
	(xref_cmd "bind")
	(setvar "bindtype" bity)
	(princ)
)

(defun c:xrbi (/ bity)
	(setq bity (getvar "bindtype"))
	(setvar "bindtype" 1)
	(xref_cmd "bind")
	(setvar "bindtype" bity)
	(princ)
)

(princ)
