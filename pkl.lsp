;	点选 line，获取长度
;	2023/10/6,	点选序号，单条长度，平均长度，总长度
;				已测试

(defun c:pkl (/ ttlng lng en ent amt)
	(setq ttlng 0 amt 0)
	(setq en (entsel "\nPick LINE object: "))
	(while (and en (= (cdr (assoc 0 (setq ent (entget (car en))))) "LINE"))
		(setq lng (distance (cdr (assoc 10 ent)) (cdr (assoc 11 ent))))
		(setq amt (1+ amt))
		(setq ttlng (+ ttlng lng))
		(setq avlng (/ ttlng amt))
		(princ "\nLength: Line ")
		(princ (rtos amt 2 0))
		(princ ": ")
		(princ (rtos lng 2 0))
		(princ ", Average: ")
		(princ (rtos avlng 2 0))
		(princ ", Total: ")
		(princ (rtos (/ ttlng 1000) 2 3))
		(princ "m. ")
		(setq en (entsel "Pick next: "))
	)
	(princ)
)

(princ "loaded. Start as C:PKL ")
(princ)