; 改变图纸比例。尚不完善。
; 2025-11-20	大幅度改写

(defun get_ss (f_list_ / en_name f_list)
	(if (setq en_name (cadr f_list_))
		(setq f_list (list (cons 0 (car f_list_)) (cons 2 en_name)))
		(setq f_list (list (cons 0 f_list_)))
	)
	(cmd "_select" ORI_SS "")
	(ssget "p" f_list)
)

(defun chsc_main (/ txt_ss dim_ss dbk_ss idx enn ent pt)
	;scale text seperately
	(setvar "highlight" 0)
	(princ "\nProccessing with Text, ")
	(setq txt_ss (get_ss '("TEXT")))
	(if txt_ss (sc_by_code txt_ss '(40)))
	;Scale Dimension Blocks
	(princ "Dimension Blocks.")
	(setq dbk_ss (get_ss '("INSERT" "AXI0,AXI,AL,AR,EL??,SY?")))
	(if dbk_ss (sc_by_code dbk_ss '(41 42 43)))
	;scale dimension
	(princ "Dimension, ")
	(setq dims (getvar "dimscale"))
	(setvar "dimscale" new_sc)
	(setq dim_ss (get_ss '("DIMENSION")))
	(cmd "_dim" "up" dim_ss "" "home" dim_ss "" "e")
	(setvar "dimscale" dims)

)

(defun sc_by_code (ss_ code_list_ / idx amt enn ent )
	(setq 	idx 0
			amt (sslength ss_)
	)
	(while (< idx amt)
		(setq  	enn (ssname ss_ idx)
				ent (entget enn)
		)
		(mapcar 
			'(lambda (code_ / old_value new_value)
				(setq 	old_value (assoc code_ ent)
						new_value (* (cdr old_value) SC)
				)
				(entmod (subst (cons code_ new_value) old_value) ent)
			)
			code_list_
		)
		(entupd enn)
		(setq idx (1+ idx))
	)
)

(defun c:chsc ()
	;(setvar "cmdecho" 0)
	(setvar "blipmode" 0)
	(prompt "\nSelect entities(Text,Dimension etc) to scale: ")
	(setq ORI_SS (ssget))
	(if ORI_SS 
		(progn
			(setq old_sc (getreal "\nOld dimscale 1/?: "))
			(initget "Scale")
			(setq new_sc (getreal "\n[Scael factor] <new dimscale 1/?>: "))
			(if (= new_sc "Scale")
				(setq 
					SC (getreal "\nScale factor: ") 
					new_sc (* old_sc SC)
				)
				(setq SC (/ new_sc old_sc))
			)
		)
	) 
	(if SC (chsc_main))
	(setvar "cmdecho" 1)
	(setvar "blipmode" 1)
	(setvar "highlight" 1)
	(princ)
)