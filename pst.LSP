; Jason Piercey . May 16th, 2003
; assign a pagesetup to a layout
; [layout] - string, layout name
; [setup] - string, pagesetup to assign
; return: T or nil
(defun putPagesetup (layout setup / layouts plots)
	(defun item-p	(collection item)
		(if
			(not (vl-catch-all-error-p
					(vl-catch-all-apply '(lambda () (setq item (vla-item collection item))))
			))
			item
		)
	)
	(and
		(or *acad* (setq *acad* (vlax-get-acad-object)))
		(or *doc* (setq *doc* (vla-get-activedocument *acad*)))
		(setq layouts (vla-get-layouts *doc*))
		(setq plots (vla-get-plotconfigurations *doc*))
		(setq layout (item-p layouts layout))
		(setq setup (item-p plots setup))
		(not (vla-copyfrom layout setup))
	)
)

(defun massoc (key alist / x nlist)
	(foreach x alist
	(if	(eq key (car x))
		(setq nlist (cons (cdr x) nlist))
	)
)
(reverse nlist)
)


; Return: list of all pagesetups defined in the current drawing or nil
(defun getPagesetups ()
	(massoc 3 (dictsearch (namedobjdict) "Acad_PlotSettings"))
)

; Jason Piercey . May 19th, 2003
; assign pagesetup to layout(s)
; LIMITED testing
; written for Shawn McDonald
(defun c:pst	(/ lst page res)
	(setq lst (mapcar 'strcase (getPagesetups)))
	(while (not page)
		(setq page (strcase (getstring T "\nspecify pagesetup to apply: ")))
		(if	(or (= "" page) (not (member page lst)))
		(progn (princ "\npagesetup not found") (setq page nil))
		)
	)

	(initget "All Current")
	(if (not (setq res (getkword "\n[All/Current]apply pagesestup to which layout(s) <all>: ")))
		(setq res "All")
	)

	(if (= "All" res)
		(foreach x (vl-remove "Model" (layoutlist))
			(putPagesetup x page)
		)
		(putPagesetup (getvar "ctab") page)
	)
	(princ "\nFinished")
	(princ)
)