; LAYEROUT.LSP
; 
; 作者：Little Fish ^\\\\\\\\\\
; 版本：1.0 (20001130)_/ _ \\\\
; 邮件：ivox@21cn.com■-■----\\\
; orca@netease.com/]\\\
; 网站：http://orca.163.net -- /// 
; http://orca.533.net ////// 酷
; 
; 本程序可以任意使用、修改、复制、交流，无版权约束；但请保留作者声明！
; 如果修改本程序，可以将成果寄给我，以便交流 ^_^
; 
; 说明：
; --------------------------------------------------------------------
; 将图形按图层分别输出为单个的图形文件。
; 本程序将有助于更正确地将AutoCAD图形输入3D Studio MAX。
; ACAD.PGP中必须定义以下一行外部命令：
; MKDIR,MKDIR,12,Folder: ,
; 这个外部命令用于建立一个子文件夹以放置输出文件。本程序会自动加入定义。
; 
; 使用方法：
; 1.加载本程序；
; 可能会跳出一个对话框，提示在ACAD.PGP中做了修改。按OK继续。
; 2.在命令行输入LAYEROUT并回车；
; 程序提示：Layer(s) to export: [Select/All]<Display>
; 参数：Select: 选择实体以确定输出的图层；
; All:输出所有图层；
; Display:输出显示的图层，忽略OFF和FROZEN图层。
; 3.如果选择Select参数，将提示选择实体。
; 4.输出的文件放置于当前文件
; 
; 注意：
; 在程序运行当中，可能会有极短暂的停顿。这是程序在通过MKDIR命令
; 创建子文件夹。
;
; =======================程序部分=================================
; 
; 程序定义
;
(defun c:layerout (/ lay_list key dwg_name itm exdwg_name exname exss ucsf ucschanged expcnt)
	(princ "\nby Little Fish. Ver1.0 （20001130）")
	(setq olderr *error*)

	;; 错误函数
	(defun *error* (s)
		(setvar "cmdecho" 0)
		(if ucschanged (cmd "_.ucs" "p"))
		(setvar "ucsfollow" (if ucsf ucsf 1))
		(setvar "expert" 0)
		(setvar "cmdecho" 1)
		(setq *error* olderr olderr nil)
		(princ)
	)

	; 获得输出方式，并得到输出的图层列表
	(setq lay_list nil)
	(initget "Select All Display")
	(setq key (getkword "\nLayer(s) to export: [Select/All]<Display> "))
	(if key nil (setq key "Display"))
	(cond
		((= key "Select")
			(setq lay_list (layerout_selected))
		)
		((= key "All")
			(setq lay_list (layerout_all))
		)
		((= key "Display")
			(setq lay_list (layerout_display))
		)
	);cond

	; 以WCS的0,0,0作为基点输出图形（可能需要改进）
	(setq dwg_name (getvar "dwgname"))
	(setq dwg_name (substr dwg_name 1 (- (strlen dwg_name) 4)))
	(setvar "expert" 2)
	(setq ucsf (getvar "ucsfollow"))
	(setvar "ucsfollow" 0)
	(setvar "cmdecho" 0)
	(cmd "_.ucs" "w")
	(setq ucschanged t)
	(setq expcnt 0)
	(foreach itm lay_list
		(progn
			(setq exdwg_name (strcat dwg_name "_layer_" itm ".dwg"))
			(setq exname (strcat (getvar "dwgprefix") exdwg_name))
			(setq exss (ssget "x" (list (cons 8 itm))))
			(if exss
				(progn
					(cmd "_.wblock" exname (if (fl_check_ver 15) "") "0,0,0" exss "")
					(cmd "oops")
					(princ (strcat "\n" itm "\t-> " exdwg_name))
					(setq expcnt (1+ expcnt))
				)
			)
		);progn
	);foreach
	(princ (strcat "\n"(rtos expcnt 2 0) " layer(s) exported. "))

	; 恢复设置，程序结束
	(if ucschanged (cmd "_.ucs" "p"))
	(setvar "ucsfollow" ucsf)
	(setvar "expert" 0)
	(setvar "cmdecho" 1)
	(setq *error* olderr olderr nil)

	(princ)
)
; 
; ==========================子程序==========================================
;
; 用于Select参数
;
(defun layerout_selected (/ ss amt idx laylist lay_name)
	(princ "\nSelect object(s) on target layer(s): ")
	(setq ss (ssget) idx 0 laylist (list))
	(setq amt (if ss (sslength ss) 0))
	(repeat amt
		(setq lay_name (cdr (assoc 8 (entget (ssname ss idx)))))
		(if (member lay_name laylist)
			nil
			(setq laylist (append laylist (list lay_name)))
		)
		(setq idx (1+ idx))
	);repeat
	laylist
)
;
; 用于All参数
;
(defun layerout_all (/ laylist lay_name)
	(setq laylist nil)
	(setq lay_name (cdr (assoc 2 (tblnext "layer" t))))
	(while lay_name
		(setq laylist (append laylist (list lay_name)))
		(setq lay_name (cdr (assoc 2 (tblnext "layer"))))
	)
	laylist
)
;
; 用于Display参数
;
(defun layerout_display (/ laylist lay_set lay_name lay_stat lay_colr);display
	(setq laylist nil)
	(setq lay_set (tblnext "layer" t))
	(while lay_set
		(setq 
			lay_name (cdr (assoc 2 lay_set))
			lay_stat (cdr (assoc 70 lay_set))
			lay_colr (cdr (assoc 62 lay_set))
		)
		(if
			(or (member lay_stat (list 69 65))
				(minusp lay_colr)
			)
			nil
			(setq laylist (append laylist (list lay_name)))
		)
		(setq lay_set (tblnext "layer"))
	)
	laylist
)
;
;===== LayerIn =========
;
(defun c:layerin (/ l_name dwg_name f_name ucsf)
	(setq layerin_lay (if layerin_lay layerin_lay "dw-id"))
	(princ (strcat "\nLayer name: <" layerin_lay ">"))
	(setq l_name (getstring))
	(setq l_name (if (= l_name "") layerin_lay))
	(setq dwg_name (getvar "dwgname"))
	(setq dwg_name (substr dwg_name 1 (- (strlen dwg_name) 4)))
	(setq f_name (strcat dwg_name "_layer_" l_name ".dwg"))
	(if (findfile f_name)
		(progn
			(setvar "cmdecho" 0)
			(setq ucsf (getvar "ucsfollow"))
			(setvar "ucsfollow" 0)
			(cmd "_.ucs" "w")
			(cmd "insert" (strcat "*" f_name) "0,0,0" "" "")
			(setvar "ucsfollow" ucsf)
			(setvar "cmdecho" 1)
			(setq layerin_lay l_name)
		)
		(alert (strcat f_name "\nnot found!"))
	)
	(princ)
)

;
; 全部定义结束，以下为加载成功提示
;
(princ "loaded. Start as C:LAYEROUT")
(princ)