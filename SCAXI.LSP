; v1.1	ABS function added.
; v2.0	可以运行于UCS, 有一些axi处理不正确。

(defun c:scaxi (/ olderr osm dims0 dims1 axi_ss axi_amt axi_idx en ent sc pt0 c1 c2 ss0 lin_en lin_ent pt1 pt2 ex)
	(prompt "\nFishLISP. 5-28-1998. Ver 2.0")

	(setq dims0 (getvar "dimscale"))
	(initget 6)
	(setq dims1 (getreal (strcat "\nDimscale: <" (rtos dims0) "> ")))
	(setq dims1 (if dims1 dims1 dims0))

	(setq axi_ss (ssget '((0 . "insert") (2 . "axi,axi0"))))
	(setq axi_amt (if axi_ss (sslength axi_ss) 0) axi_idx 0)
	(setq osm (getvar "osmode"))
	(setvar "highlight" 0)
	(setvar "cmdecho" 0)
	(setvar "blipmode" 0)
	(setvar "osmode" 0)

	(cssave)
	
	(repeat axi_amt
		(setq 
			en (ssname axi_ss axi_idx)
			ent (entget en)
			sc (abs (cdr (assoc 41 ent)))
			pt0 (cdr (assoc 10 ent))
			c1 (polar pt0 (* pi 0.75) (* sc 8.0))
			c2 (polar pt0 (* pi 1.75) (* sc 8.0))
		)
		(setq ss0 (ssget "c" c1 c2))
		(if ss0 (setq ss0 (ssget "p" '((0 . "line") (8 . "paxinum,paxis")))))
		(if ss0
			(setq 
				lin_en (ssname ss0 0)
				lin_ent (entget lin_en)
				pt1 (cdr (assoc 10 lin_ent))
				pt2 (cdr (assoc 11 lin_ent))
				pt0 (if (< (distance pt0 pt1) (distance pt0 pt2)) pt1 pt2)
			)
		)
		(setvar "cmdecho" 0)
		(cmd "scale" en "" pt0 "r" sc dims1)
		(setq axi_idx (1+ axi_idx))
	)

	(csrestore)
	(setvar "cmdecho" 1)
	(setvar "blipmode" 1)
	(setvar "highlight" 1)
	(setvar "osmode" osm)
	(princ)
)

(princ "loaded. Start as C:SCAXI. ")
(princ)