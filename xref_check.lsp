; 外部参考叠图比对
; 思路
;	列出外部参考，通过序号选择，仅一个则直接选取
;	将所有XREF图层改为红色，并关闭
;	打开一个XREF图层，显示图层名称；
;	next
;	可以考虑对图层列表先作过滤，排除标注等图层
; 2024-11/22	0.1	思路

(defun check_xref()
)

(defun list_xref()
)

(defun get_xref(li)
)

(defun list_xref_layers(xr)
)

(defun change_xref_layer_state(la)
)

(defun c:xref_show()
	(setq xref_list (list_xref))
	(setq xref_comp (get_xref(xref_list)))
	(setq xref_lays (list_xref_layers(xref_comp)))
	(foreach lay xref_lays change_xref_layer_state(lay))
)