; 图块替换，非常有用；
; 1996-12-26	v1.0.

(defun chblk (blkname / ins_name en1 ins_ss idx en ent)
	(setq ins_name (getstring "\nInsert name, or RETURN to pick: "))
	(if (= ins_name "") 
		(setq en1 (entsel "\nPick an insert to change: "))
		(progn
			(prompt "\nSelect insert entities to change: ")
			(setq ins_ss (ssget (list '(0 . "insert") (cons 2 ins_name))))
			(if ins_ss nil (princ "\nInsert not found."))
		)
	)
	(if en1 
		(progn
			(setq ins_name (cdr (assoc 2 (entget (car en1)))))
			(prompt "\nSelect insert entities to change: ")
			(setq ins_ss (ssget (list '(0 . "insert") (cons 2 ins_name))))
			(if ins_ss nil (princ "\nInsert not found."))
		)
	)
	(if ins_ss
		(progn
			(setq idx 0)
			(repeat (sslength ins_ss)
				(setq 
					en (ssname ins_ss idx)
					ent (entget en)
				)
				(entmod (subst (cons 2 blkname) (assoc 2 ent) ent))
				(entupd en)
				(setq idx (1+ idx))
			)
		)
	)
)
 
(defun c:chbk (/ blkname)
	(princ "\nFishLISP, C:CHBK")
	(if chblk_name 
		(progn
			(princ (strcat "\nBlock name <" chblk_name))
			(setq blkname (getstring ">: "))
			(if (= blkname "")
				(setq blkname chblk_name)
			)
		)
		(setq blkname (getstring "\nBlock name: "))
	)
	(if (and (/= blkname "") (tblsearch "block" blkname))
		(progn
			(chblk blkname)
			(setq chblk_name blkname)
		)
		(if (/= blkname "")
			(princ "\nBlock not defined.")
		)
	)
	(princ)
) 