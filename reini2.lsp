;Layer name Color Linetype
;---------------------------------------------
;07 (white) CONTINUOUS
;ANNO 1 (red) CONTINUOUS
;AREA 1 (red) CONTINUOUS
;CLM-F2 (yellow)CONTINUOUS
;CLM-H134 CONTINUOUS
;CLM-L1 (red) CONTINUOUS
;DOOR30 CONTINUOUS
;DW-ID2 (yellow)CONTINUOUS
;PAXIS134 ACAD_ISO04W100
;PDIM 3 (green) CONTINUOUS
;PDIM1136 CONTINUOUS
;PDIM2 40 CONTINUOUS
;T4 CONTINUOUS
;VP 252 CONTINUOUS
;WALL41 CONTINUOUS
;WINDOW 150 CONTINUOUS
;WL31 CONTINUOUS

; History:
; 2025-11-21	rewrite with assoc list
; 2003-06-16: Bugfix
; WD-ID text objects filter
; 2003-03-24: update
; treat F or L layer(s) OK!
; more filter for text and other objects type.
; layer color and linetype changed.

(fl_undo_begin)
(setvar "regenmode" 0)

; Check Acad_iso04w100
; not for new version
;(if (tblsearch "ltype" "acad_iso04w100")
;nil
;(cmd "_linetype" "load" "acad_iso04w100" "acad.lin" "")
;)

(load "fllt.lsp")

(setq rename_list
	(list 
		'("1-空调-30" 	"设备 空调")
		'("251" 		"填充 30%")
		'("WINDOW_TEXT" "文字 门窗")
		'("STAIR" 		"看线 0")
		'("厨卫" 		"布置 厨具")
		'("空调" 		"设备 空调")
		'("厨房" 		"布置 厨具")
		'("卫生间" 		"布置 洁具")
		(cons "AREA" 	FLLT_AREA)
		(cons "COLUMN" 	FLLT_COL)
		(cons "dote" 	FLLT_AXIS)
		(cons "fur" 	FLLT_FUR)
		(cons "WALL" 	FLLT_WALL)
		(cons "WINDOW" 	FLLT_WINDOW)
	)
)

(defun rename_layer (old_name_ / itm new_name)
	(if (setq itm (assoc old_name_ rename_list))
		(progn 
			(setq new_name (cdr itm))
			(setvar "cmdecho" 0)
			(cmd "_layer" "rename" old_name_ new_name "")
			(setvar "cmdecho" 1)
		)
	)
)

; save layer F/T L/U state
(setq 	lay_f_str (get_laystr_by_state "freeze")
		lay_l_str (get_laystr_by_state "lock")
)

(setvar "cmdecho" 0)
(cmd "_layer" "t" "*" "")
(cmd "_layer" "u" "*" "")

(setq lay (tblnext "layer" t))
(while lay
	(rename_layer (cdr (assoc 2 lay)))
	(setq lay (tblnext "layer"))
)

; Make layer function
(defun make_layer (laname_ laclr_ laltype_)
	(cmd "_layer" "m" laname_ "c" laclr_ "" "l" laltype_ "" "")
)

; Make layers
(princ "\n重新设置作图环境")
(if (= (getvar "cvport") 1)
	(cmd "_mspace")
)
(setq clay (getvar "clayer"))
(princ "\n\t设置缺省图层")

(make_layer "1"	"9"	"")
(make_layer "defpoints" "5" "")
(make_layer "布置 厨具"	"4"	"")
(make_layer "布置 洁具"	"4"	"")
(make_layer "看线 0" 	"9" "")
(make_layer "栏杆" 		"3" "")
(make_layer "设备 空调"	"3"	"")
(make_layer "填充 15%" 	"251" "")
(make_layer "填充 30%" 	"252" "")
(make_layer "文字 门窗"	"2"	"")
(make_layer "文字 名称"	"4"	"")
(make_layer FLLT_AREA 	"1"	"")
(make_layer FLLT_AXIS 	"134" "")
(make_layer FLLT_COL 	"6" "")
(make_layer FLLT_DIM0 	"3" "")
(make_layer FLLT_FUR	"252"	"")
(make_layer FLLT_WALL 	"41" "")
(make_layer FLLT_WINDOW "150" "")

(setvar "clayer" clay)

(setvar "highlight" 0)
(setq 
	ss_d (ssget "X" (list '(0 . "DIMENSION,LEADER") '(-4 . "<NOT")(cons 8 FLLT_DIM0)'(-4 . "NOT>")))
	ss_t (ssget "X" (list '(0 . "TEXT,MTEXT") '(-4 . "<NOT") '(8 . "文字 门窗,文字 名称,0")' (-4 . "NOT>")))
	ss_a (ssget "X" (list '(0 . "TEXT,MTEXT") '(8 . "标注*")))
	ss_n (ssget "X" (list '(0 . "INSERT") '(2 . "AXI,AXI0")))
	ss_f (ssget "x" (list '(0 . "INSERT") '(2 . "ZQ?,?YP,?SP,??C?,?c?")))
	ss_e (ssget "x" (list '(0 . "INSERT") '(2 . "EL??")))
	ss_w (ssget "x" (list '(0 . "TEXT") '(1 . "C-*,M-*,FM-*,MC-*,LM-*,FC-*,(C-*,(M-*,(FM-*,(MC-*,(LM-*,(FC-*")))
)

(if (or ss_d ss_e)
	(princ "\n\t转换标注实体Dimension的图层; ")
)
(if ss_e (cmd "_chprop" ss_e "" "la" FLLT_DIM0 ""))
(if ss_d (cmd "_chprop" ss_d "" "la" FLLT_DIM0 ""))
;(if ss_a (cmd "_chprop" ss_a "" "c" "7" ""))

(if ss_n (princ "\n\t转换轴线号的图层; "))
(if ss_n (cmd "_chprop" ss_n "" "la" FLLT_AXIS ""))

(if ss_t (princ "\n\t转换文本实体Text的图层; "))
(if ss_t (cmd "_chprop" ss_t "" "la" "文字 名称" ""))
(if ss_w (cmd "_chprop" ss_w "" "la" "文字 门窗" ""))

(if ss_f (princ "\n\t转换特定图块的图层; "))
(if ss_f (cmd "_chprop" ss_f "" "la" "fur" ""))

(cmd "_layer" "plot" "n" "面积" "")

(if lay_f_str (cmd "_layer" "f" lay_f_str ""))
(if lay_l_str (cmd "_layer" "l" lay_l_str ""))

(redraw)
(setvar "highlight" 1)
(setvar "clayer" clay)
(setvar "regenmode" 1)
(setvar "cecolor" "BYLAYER")
(fl_undo_end)
(setvar "cmdecho" 1)

;(if (findfile "archdim.lsp")
;(progn
;(princ "\n\t重设标注风格DIMSTYLE")
;(load "archdim")
;(princ "\n完成。")
;)
;)

(princ)

