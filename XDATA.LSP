(defun c:xdata (/ app data)
	(if #app nil (setq #app "ACAD"))
	(princ "\nApplication entry name <")
	(princ #app)
	(setq app (getstring ">:"))
	(if (= app "") 
		(setq app #app)
	)
	(if (tblsearch "appid" app)
		(progn
			(setq app (strcase app) #app app)
			(setq data (cadr (assoc -3 (entget (car (entsel)) (list app)))))
			(if data 
				(princ data)
				(alert "No such XDATA append to this object.")
			)
		)
		(alert "No such Application entry registered.")
	)
	(princ)
)

(defun c:applist (/ app)
	(textscr)
	(setq app (tblnext "appid" t))
	(if app nil (alert "No Application entry registered."))
	(while app
		(princ "\n\t")
		(princ (cdr (assoc 2 app)))
		(setq app (tblnext "appid"))
	)
	(princ)
)