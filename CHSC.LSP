; 改变图纸比例。尚不完善。

(defun tkchsc (sc / txt_ss dm_ss dim_ss pl_ss pl plw count entn ent pt)

;scale text seperately
	(setvar "highlight" 0)
	(princ "\nProccessing with Text, ")
	(setq txt_ss (ssget "p" '((0 . "TEXT")))) 
	(if txt_ss (txthi sc txt_ss))

;scale dimension
	(princ "Dimension, ")
	(setq dims (getvar "dimscale"))
	(setvar "dimscale" nwsc)
	(cmd "_select" ss "r" txt_ss "")
	(setq ss (ssget "p"))
	(setq dm_ss (ssget "p" '((0 . "dimension"))))
	(cmd "_dim" "up" dm_ss "" "home" dm_ss "" "e")
	(setvar "dimscale" dims)

;Scale Dimension Blocks
	(princ "Dimension Blocks.")
	(cmd "_select" ss "r" dm_ss "")
	(setq ss (ssget "p"))
	(setq dim_ss (ssget "p" '((2 . "AXI0,AXI,AL,AR,EL??,SY?") (0 . "INSERT"))))
	(setq count 0)
	(if dim_ss
		(while (< count (sslength dim_ss))
			(setq 
				entn (ssname dim_ss count)
				ent (entget entn)
				pt (cdr (assoc 10 ent))
			)
			(cmd "scale" entn "" pt sc)
			(setq count (1+ count))
		)
	)
	
;Change Polyline Width
	(cmd "_select" ss "r" dim_ss "")
	(setq 
		pl_ss (ssget "p" '((0 . "POLYLINE"))) 
		idx 0
	)
	(if pl_ss 
		(if (findfile "plw2.lsp")
			(load "plw2")
			(alert "A module not found\nCannot change Polylines' width")
		)
	)
	(if plwsc 
		(repeat (sslength pl_ss)
			(setq en (ssname pl_ss idx))
			(plwsc en)
			(setq idx (1+ idx))
		)
	)

)

(defun txthi (sc ss / count entn ent hi_old hi_new ent_nw)
	(setq count 0)
	(while (< count (sslength ss))
		(setq 
			entn (ssname ss count)
			ent (entget entn)
			hi_old (cdr (assoc 40 ent))
			hi_new (* hi_old sc)
		)
		(entmod (subst (cons 40 hi_new) (assoc 40 ent) ent))
		(setq count (1+ count))
	)
	(princ)
)

(defun c:chsc ()
; (setvar "cmdecho" 0)
	(setvar "blipmode" 0)
	(cmd "_layer"
		"t" "t,pdim,paxinum,wl"
		"u" "t,pdim,paxinum,wl"
		"on" "t,pdim,paxinum,wl" ""
	)
	(prompt "\nSelect entities, including Text,Dimension etc. to edit: ")
	(setq ss (ssget))
	(if ss 
		(progn
			(setq odsc (getreal "\nOld dimscale 1/?: "))
			(initget "Scale")
			(setq nwsc (getreal "\n[Scael factor] <new dimscale 1/?>: "))
			(if (= nwsc "Scale")
				(setq sc (getreal "\nScale factor: ") nwsc (* odsc sc))
				(setq sc (/ nwsc odsc))
			)
		)
	) 
	(if sc (tkchsc sc))
	(setvar "cmdecho" 1)
	(setvar "blipmode" 1)
	(setvar "highlight" 1)
	(princ)
)