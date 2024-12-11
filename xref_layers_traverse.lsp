; �ⲿ�ο���ͼ��ͼ������ȶ�

; ˼·��
;	ͨ����ѡ��ʽֱ��ѡȡ XREF ͼ��
;	������ XREF ͼ���Ϊ��ɫ�����ر�
;	��һ�� XREF ͼ�㣬��ʾͼ�����ƣ�
;	next
;	��ɺ�ص� UNDO MARK
;	���Կ��Ƕ�ͼ���б��������ˣ��ų���ע��ͼ��

; ��ʷ��
; 	2024-11-22	0.1	˼·
; 	2024-12-11	0.2 �����ܹ�������ͨ��
; -----------------------------------------------------------------

; �û�������
; �ų������ʵ��ͼֽ���������Ҫ��ȫ����д
; ���б��ڹؼ��ֵ�ͼ�㣬�����رգ����������
(setq exclude_list (list "DIM" "AXI" "TEXT" "TXT" "MARK"))

; -----------------------------------------------------------------

; ��������

; �ж��Ƿ��ų���
(defun is_excluded (l_name); bool
	(apply 'or
		(mapcar
			(lambda (str) (wcmatch (strcase l_name) (strcat "*" str "*")))
			exclude_list
		)
	)
)

; ͨ����ѡ��ʽֱ��ѡȡ 2024-12-11
; 2024-12-11 tested
(defun select_xref () ; ���� xref ͼ������
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

; ���XREFͼ��
; 2024-12-11 tested
(defun list_xref_layers (b_name / la_list la la_name) ; ���ذ��� xref ͼ�������ͼ����б�
	(defun *error* (msg)
		(princ "\nlist_xref_layers function error:\t")
		(princ msg)
	)
	(setq la_list (list))
	(setq la (tblnext "layer" t))
	(while la
		(setq la_name (cdr (assoc 2 la)))
		(if (= (logand (cdr (assoc 70 la)) 1) 1); ����ͼ�㲻����
			nil
			(if (wcmatch la_name (strcat b_name "*"))
				(if (is_excluded la_name); �ų���ע��ͼ��
					nil
					(setq la_list (append la_list (list la_name)))
				)
			)
		)
		(setq la (tblnext "layer"))
    )
	la_list
)

; ������XREFͼ���Ϊ��ɫ
(defun layers_2_red (b_name) ; nil
	(command "_.layer" "c" "r" (strcat b_name "*") "")
)

; ������XREFͼ��ر�
(defun layers_off (b_name) ; nil
	(command "_.layer" "off" (strcat b_name "*") "")
)

; ��һ��XREFͼ�㣬��ʾͼ������; Next
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