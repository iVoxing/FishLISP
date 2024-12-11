; 外部参考叠图，图层遍历比对

; 思路：
;	通过点选方式直接选取 XREF 图块
;	将所有 XREF 图层改为红色，并关闭
;	打开一个 XREF 图层，显示图层名称；
;	next
;	完成后回到 UNDO MARK
;	可以考虑对图层列表先作过滤，排除标注等图层

; 历史：
; 	2024-11-22	0.1	思路
; 	2024-12-11	0.2 完整架构，调试通过
; -----------------------------------------------------------------

; 用户定制项
; 排除项，根据实际图纸情况调整，要求全部大写
; 含有表内关键字的图层，将被关闭，不参与遍历
(setq exclude_list (list "DIM" "AXI" "TEXT" "TXT" "MARK"))

; -----------------------------------------------------------------

; 程序内容

; 判断是否排除项
(defun is_excluded (l_name); bool
	(apply 'or
		(mapcar
			(lambda (str) (wcmatch (strcase l_name) (strcat "*" str "*")))
			exclude_list
		)
	)
)

; 通过点选方式直接选取 2024-12-11
; 2024-12-11 tested
(defun select_xref () ; 返回 xref 图块名称
	(setq obj (entsel "\nSelect a Xref Insert: "))
	(if obj 
		(setq ent (entget (car obj))
			block_name (cdr (assoc 2 ent))
			block_def (tblsearch "block" block_name)
		)
	)
	(if block_def
		(setq block_type (cdr (assoc 70 block_def)))
  	)
  	(if (= (logand block_type 4) 4)
    	block_name
  	)
)

; 获得XREF图层
; 2024-12-11 tested
(defun list_xref_layers (b_name / la_list la la_name) ; 返回包含 xref 图块的所有图层的列表
	(defun *error* (msg)
		(princ "\nlist_xref_layers function error:\t")
		(princ msg)
	)
	(setq la_list (list))
	(setq la (tblnext "layer" t))
	(while la
		(setq la_name (cdr (assoc 2 la)))
		(if (= (logand (cdr (assoc 70 la)) 1) 1); 冻结图层不处理
			nil
			(if (wcmatch la_name (strcat b_name "*"))
				(if (is_excluded la_name); 排除标注等图层
					nil
					(setq la_list (append la_list (list la_name)))
				)
			)
		)
		(setq la (tblnext "layer"))
    )
	la_list
)

; 将所有XREF图层改为红色
(defun layers_2_red (b_name) ; nil
	(command "_.layer" "c" "r" (strcat b_name "*") "")
)

; 将所有XREF图层关闭
(defun layers_off (b_name) ; nil
	(command "_.layer" "off" (strcat b_name "*") "")
)

; 打开一个XREF图层，显示图层名称; Next
; 2024-12-11, tested
(defun show_layer (b_name l_list / get_lay lay k amt idx); nil
	(setq amt (length l_list) idx 1)
	(setq lay (car l_list) l_list (cdr l_list))
	(while lay
		(layers_off b_name)
		(command "_.layer" "on" lay "")
		(initget "Next Exit")
		(setq k (getkword (strcat "\nLayer " (rtos idx) "/" (rtos amt) ": " lay ". Exit<Next>: ")))
		(setq k (if k k "Next"))
        (if (= k "Next")
			(setq lay (car l_list) l_list (cdr l_list))
			(setq lay nil)
        )
		(setq idx (1+ idx))
		(redraw)
	)
)

(defun c:xlt (/ block_name layer_list) 
	(defun *error* (msg)
		(command "undo" "back")
		(setvar "cmdecho" 1)
		(princ "\nError: ")
		(princ msg)
	)
	(setvar "cmdecho" 0)
	(command "undo" "mark")
	(setq block_name (select_xref))
	(if block_name
		(setq layer_list (list_xref_layers block_name))
	)
	(if layer_list
		(progn
			(layers_2_red block_name)
			(show_layer block_name layer_list)
		)
	)
	(command "undo" "back")
	(setvar "cmdecho" 1)
)

(c:xlt)
(princ)