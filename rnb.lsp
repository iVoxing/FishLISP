; FishLISP C:RNB
; 通过点击实体，更改图块名称。避免查询、输入原有名称。
;
; 在AutoCAD当中，可以很方便地复制一组实体，并粘贴成图块。
; 但所得到的图块总是具有如“A$C4FD24C92”之类复杂的名称，
; 往往并不利于识别和操作，所以有时候有必要将这样的图块更
; 名；而其名称的复杂性，同样使得更名也并不容易。
; C:RNB则避免了查询、输入图块名称，使更名极为便利。
;
; 程序支持长名称（即名称中允许输入空格），所以新名称必须
; 靠回车来确认。
;-----------------------------------------------------
; 2004-07-07v1.0可用版
;-----------------------------------------------------

(defun c:rnb (/ en ent ety ena enn)
	(princ "\nFishLISP C:RNB v1.0.")
	(if (setq en (entsel "\n选取要改名的图块："))
		(if en
			(progn
				(setq 
					ent (entget (car en))
					ety (cdr (assoc 0 ent))
				)
				(if (= ety "INSERT")
					(progn
						(setq ena (cdr (assoc 2 ent)))
						(setq loop t)
						(while loop
							(princ )
							(princ )
							(setq enn (getstring 1 (strcat "\n图块名称：<" ena "> ")))
							(if (member enn (list "" " " ena))
								(setq loop nil enn ena)
								(if (tblsearch "block" enn)
									(princ "\n已经存在此名称的图块。")
									(setq loop nil)
								)
							)
						)
						(setvar "cmdecho" 0)
						(cmd "rename" "block" ena enn)
						(setvar "cmdecho" 1)
					)
					(princ "\n对象不是图块。")
				)
			)
		)
	)
	(princ)
)

(princ)

