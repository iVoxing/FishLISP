; 图块替换，非常有用；
; 2025-11-16	rewrited 
; 1996-12-26	v1.0.

(defun chblk (block_name_ / idx en ent)
	(setq idx 0)
	(repeat (sslength ins_ss)
		(setq 
			en (ssname ins_ss idx)
			ent (entget en)
		)
		(entmod (subst (cons 2 block_name_) (assoc 2 ent) ent))
		(entupd en)
		(setq idx (1+ idx))
	)
)
 
(defun c:chbk (/ blkname ins_name en_picked ins_ss)
	(if CHBLK_NAME
		(setq 
			blkname (getstring (strcat "\nBlock name: <" CHBLK_NAME ">"))
			blkname (if (= blkname "") CHBLK_NAME blkname)
		)
		(setq blkname (getstring "\nBlock name: "))
	)
	(if (tblsearch "block" blkname)
		(progn
			(setq CHBLK_NAME blkname)
			(setq ins_name (getstring "\nInsert name: <Pick an insert>"))
			(if (= ins_name "") 
				(setq en_picked (entsel "\nPick an insert to get name: "))
				(progn
					(prompt (strcat "\nSelect [" ins_name "] to change: "))
					(setq ins_ss (ssget (list '(0 . "insert") (cons 2 ins_name))))
					(if ins_ss nil (princ "\nInsert not found."))
				)
			)
			(if en_picked 
				(progn
					(setq ins_name (cdr (assoc 2 (entget (car en_picked)))))
					(prompt (strcat "\nSelect [" ins_name "] to change: "))
					(setq ins_ss (ssget (list '(0 . "insert") (cons 2 ins_name))))
				)
			)
			(if ins_ss 
				(chblk blkname) 
				(princ "\nInsert not found.")
			)
		)
		(if (/= blkname "")
			(princ "\nBlock not defined.")
		)
	)
	(princ)
)

(princ "\nFishLISP. C:CHBK ")
(princ)