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
; 2003-06-16: Bugfix
; WD-ID text objects filter
; 2003-03-24: update
; treat F or L layer(s) OK!
; more filter for text and other objects type.
; layer color and linetype changed.

(setvar "cmdecho" 0)
(cmd "undo" "begin")
(setvar "regenmode" 0)

; Check Acad_iso04w100
; not for new version
;(if (tblsearch "ltype" "acad_iso04w100")
;nil
;(cmd "linetype" "load" "acad_iso04w100" "acad.lin" "")
;)

; save layer F/T L/U state
(setq oldname_list (list "WALL"	"WINDOW"	"WINDOW_TEXT"	"AREA"	"COLUMN"	"251"		"dote"	"1-�յ�-30"	"����"		"STAIR"		"�յ�"		"����"		"������" ))
(setq newname_list (list "ǽ��"	"�Ŵ�"		"���� �Ŵ�"	"���"	"�ṹ��"	"��� 30%"	"����"	"�豸 �յ�"	"���� ����"	"���� 0"	"�豸 �յ�"	"���� ����"	"���� ���"))
(defun rnla (oldname)
	(if (member oldname oldname_list)
		(cmd "layer" "rename" oldname (nth (- (length oldname_list) (length (member oldname oldname_list))) newname_list) "")
	)
)
(setq 
	lay_f_list (list)
	lay_l_list (list)
	lay (tblnext "layer" t)
)
(while lay
	(if (= (logand (cdr (assoc 70 lay)) 1) 1)
		(setq lay_f_list (append lay_f_list (list (cdr (assoc 2 lay)))))
	)
	(if (= (logand (cdr (assoc 70 lay)) 4) 4)
		(setq lay_l_list (append lay_l_list (list (cdr (assoc 2 lay)))))
	)
	(rnla (cdr (assoc 2 lay)))
	(setq lay (tblnext "layer"))
)
(cmd "layer" "t" "*" "")
(cmd "layer" "u" "*" "")

; Make layer function
(defun rsla (laname laclr laltype)
	(cmd "layer" "m" laname "c" laclr "" "l" laltype "" "")
)

; Make layers
(princ "\n����������ͼ����")
(if (= (getvar "cvport") 1)
	(cmd "mspace")
)
(setq clay (getvar "clayer"))
(princ "\n\t����ȱʡͼ��")
(rsla "1"	"9"	"")
(rsla "���"	"1"	"")
(rsla "�ṹ��" "6" "")
(rsla "��� 15%" "251" "")
(rsla "��� 30%" "252" "")
(rsla "defpoints" "5" "")
(rsla "�Ŵ�"	"150"	"")
(rsla "���� �Ŵ�"	"2"	"")
(rsla "���� �Ҿ�"	"252"	"")
(rsla "����"	"134"	"")
(rsla "��ע 0"	"3"	"")
(rsla "���� ����"	"4"	"")
(rsla "���� ���"	"4"	"")
(rsla "ǽ��"	"41"	"")
(rsla "���� ����"	"4"	"")
(rsla "���� 0"	"9"	"")
(rsla "�豸 �յ�"	"3"	"")
(rsla "����"	"3"	"")


(setvar "highlight" 0)
(setq 
	ss_d (ssget "X" '((0 . "DIMENSION,LEADER") (-4 . "<NOT")(8 . "��ע 0")(-4 . "NOT>")))
	ss_t (ssget "X" '((0 . "TEXT,MTEXT") (-4 . "<NOT")(8 . "���� �Ŵ�,���� ����,0")(-4 . "NOT>")))
	ss_a (ssget "X" '((0 . "TEXT,MTEXT") (8 . "PDIM*")))
	ss_n (ssget "X" '((0 . "INSERT") (2 . "AXI,AXI0")))
	ss_f (ssget "x" '((0 . "INSERT") (2 . "ZQ?,?YP,?SP,??C?,?c?")))
	ss_e (ssget "x" '((0 . "INSERT") (2 . "EL??")))
	ss_p (ssget "x" '((0 . "LINE") (8 . "PAXINUM")))
	ss_w (ssget "x" '((0 . "TEXT") (1 . "C-*,M-*,FM-*,MC-*,LM-*,FC-*,(C-*,(M-*,(FM-*,(MC-*,(LM-*,(FC-*")))
)

(if ss_d (princ "\n\tת����עʵ��Dimension��ͼ��; "))

(if ss_e (cmd "chprop" ss_e "" "la" "��ע 0" ""))
(if ss_d (cmd "chprop" ss_d "" "la" "��ע 0" ""))
;(if ss_a (cmd "chprop" ss_a "" "c" "7" ""))

;(if ss_n (princ "\n\tת�����ߺŵ�ͼ��; "))
;(if ss_n (cmd "chprop" ss_n "" "la" "paxinum" ""))
;(if ss_p (cmd "chprop" ss_p "" "la" "paxis" ""))

(if ss_t (princ "\n\tת���ı�ʵ��Text��ͼ��; "))
(if ss_t (cmd "chprop" ss_t "" "la" "���� ����" ""))
(if ss_w (cmd "chprop" ss_w "" "la" "���� �Ŵ�" ""))

;(if ss_f (princ "\n\tת���ض�ͼ���ͼ��; "))
;(if ss_f (cmd "chprop" ss_f "" "la" "fur" ""))

(cmd "layer" "plot" "n" "���" "")

(if lay_f_list 
	(foreach itm lay_f_list (cmd "layer" "f" itm ""))
)
(if lay_l_list
	(foreach itm lay_l_list (cmd "layer" "lo" itm ""))
)

(redraw)
(setq rsla nil ss_d nil ss_t nil ss_n nil ss_f nil ss_l nil l_id nil en nil ss_w nil)
(setvar "highlight" 1)
(setvar "clayer" clay)
(setvar "regenmode" 1)
(setvar "cecolor" "BYLAYER")
(cmd "undo" "end")
(setvar "cmdecho" 1)

;(if (findfile "archdim.lsp")
;(progn
;(princ "\n\t�����ע���DIMSTYLE")
;(load "archdim")
;(princ "\n��ɡ�")
;)
;)

(princ)

