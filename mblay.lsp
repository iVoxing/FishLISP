; merge binded layer 

(defun c:mblay ()
	(tblnext "layer" t)
	(setq lay_itm (tblnext "layer"))
	(while (wcmatch (cdr (assoc 2 lay_itm)) "*$0$*")
)

(princ " loaeded. Start as C:MBLAY ")
(princ)