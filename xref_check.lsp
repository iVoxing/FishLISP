; �ⲿ�ο���ͼ�ȶ�
; ˼·
;	�г��ⲿ�ο���ͨ�����ѡ�񣬽�һ����ֱ��ѡȡ
;	������XREFͼ���Ϊ��ɫ�����ر�
;	��һ��XREFͼ�㣬��ʾͼ�����ƣ�
;	next
;	���Կ��Ƕ�ͼ���б��������ˣ��ų���ע��ͼ��
; 2024-11/22	0.1	˼·

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