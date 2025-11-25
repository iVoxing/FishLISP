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
;PAXINUM7 (white) CONTINUOUS
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

(cmd "undo" "begin")
(setvar "regenmode" 0)

; Check Acad_iso04w100
; not for new version
;(if (tblsearch "ltype" "acad_iso04w100")
;nil
;(cmd "linetype" "load" "acad_iso04w100" "acad.lin" "")
;)

(setq rename_list
	(list 
		'("1-空调-30" "设备 空调")
		'("251" "填充 30%")
		'("AREA" "面积")
		'("COLUMN" "结构柱")
		'("dote" "轴线")
		'("fur" "布置 家具")
		'("WALL" "墙体")
		'("WINDOW" "门窗")
		'("WINDOW_TEXT" "文字 门窗")
		'("STAIR" "看线 0")
		'("厨卫" "布置 厨具")
		'("空调" "设备 空调")
		'("厨房" "布置 厨具")
		'("卫生间" "布置 洁具")
	)
)

(defun rename_layer (name_ / itm old_name new_name)
	(if (setq itm (assoc old_name rename_list))
		(progn 
			(setq new_name (cdr itm))
			(cmd "layer" "rename" old_name new_name "")
		)
	)
)

; save layer F/T L/U state
(setq 	lay_f_str (get_laystr_by_state "freeze")
		lay_l_str (get_laystr_by_state "lock")
)

(setvar "cmdecho" 0)
(cmd "layer" "t" "*" "")
(cmd "layer" "u" "*" "")

(setq lay (tblnext "layer" t))
(while lay
	(rename_layer (cdr (assoc 2 lay)))
	(setq lay (tblnext "layer"))
)

; Make layer function
(defun make_layer (laname_ laclr_ laltype_)
	(cmd "layer" "m" laname_ "c" laclr_ "" "l" laltype_ "" "")
)

; Make layers
(princ "\n重新设置作图环境")
(if (= (getvar "cvport") 1)
	(cmd "mspace")
)
(setq clay (getvar "clayer"))
(princ "\n\t设置缺省图层")
(make_layer "1"	"9"	"")
(make_layer "defpoints" "5" "")
(make_layer "标注 0" "3" "")
(make_layer "布置 厨具"	"4"	"")
(make_layer "布置 家具"	"252"	"")
(make_layer "布置 洁具"	"4"	"")
(make_layer "结构柱" "6" "")
(make_layer "看线 0" "9" "")
(make_layer "栏杆" "3" "")
(make_layer "门窗" "150" "")
(make_layer "面积" "1"	"")
(make_layer "墙体" "41" "")
(make_layer "设备 空调"	"3"	"")
(make_layer "填充 15%" "251" "")
(make_layer "填充 30%" "252" "")
(make_layer "文字 门窗"	"2"	"")
(make_layer "轴线" "134" "")
(make_layer "文字 名称"	"4"	"")
(setvar "clayer" clay)

(setvar "highlight" 0)
(setq 
	ss_d (ssget "X" '((0 . "DIMENSION,LEADER") (-4 . "<NOT")(8 . "标注 0")(-4 . "NOT>")))
	ss_t (ssget "X" '((0 . "TEXT,MTEXT") (-4 . "<NOT")(8 . "文字 门窗,文字 名称,0")(-4 . "NOT>")))
	ss_a (ssget "X" '((0 . "TEXT,MTEXT") (8 . "PDIM*")))
	ss_n (ssget "X" '((0 . "INSERT") (2 . "AXI,AXI0")))
	ss_f (ssget "x" '((0 . "INSERT") (2 . "ZQ?,?YP,?SP,??C?,?c?")))
	ss_e (ssget "x" '((0 . "INSERT") (2 . "EL??")))
	ss_p (ssget "x" '((0 . "LINE") (8 . "PAXINUM")))
	ss_w (ssget "x" '((0 . "TEXT") (1 . "C-*,M-*,FM-*,MC-*,LM-*,FC-*,(C-*,(M-*,(FM-*,(MC-*,(LM-*,(FC-*")))
)

(if ss_d (princ "\n\t转换标注实体Dimension的图层; "))
(if ss_e (cmd "chprop" ss_e "" "la" "标注 0" ""))
(if ss_d (cmd "chprop" ss_d "" "la" "标注 0" ""))
;(if ss_a (cmd "chprop" ss_a "" "c" "7" ""))

;(if ss_n (princ "\n\t转换轴线号的图层; "))
;(if ss_n (cmd "chprop" ss_n "" "la" "paxinum" ""))
;(if ss_p (cmd "chprop" ss_p "" "la" "paxis" ""))

(if ss_t (princ "\n\t转换文本实体Text的图层; "))
(if ss_t (cmd "chprop" ss_t "" "la" "文字 名称" ""))
(if ss_w (cmd "chprop" ss_w "" "la" "文字 门窗" ""))

;(if ss_f (princ "\n\t转换特定图块的图层; "))
;(if ss_f (cmd "chprop" ss_f "" "la" "fur" ""))

(cmd "layer" "plot" "n" "面积" "")

(if lay_f_str (cmd "layer" "f" lay_f_str ""))
(if lay_l_str (cmd "layer" "l" lay_l_str ""))

(redraw)
(setvar "highlight" 1)
(setvar "clayer" clay)
(setvar "regenmode" 1)
(setvar "cecolor" "BYLAYER")
(cmd "undo" "end")
(setvar "cmdecho" 1)

;(if (findfile "archdim.lsp")
;(progn
;(princ "\n\t重设标注风格DIMSTYLE")
;(load "archdim")
;(princ "\n完成。")
;)
;)

(princ)

