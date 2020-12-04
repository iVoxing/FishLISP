(set 'cmd (if (type command-s) command-s command))

; ��������ֵ
(defun fl_exchange (var1 var2 / temp) 
	(setq 
		temp var1
		var1 var2
		var2 temp
	)
)

;����UNDO���
(defun fl_undo_begin (/ cmdstat) 
	(setq cmdstat (getvar "cmdecho"))
	(setvar "cmdecho" 0)
	(if (= (substr (getvar "acadver") 1 2) "12") 
		(cmd "undo" "group")
		(cmd "undo" "begin")
	)
	(setvar "cmdecho" cmdstat)
)

;����UNDO�յ�
;�����������ERROR����ʱ������ʹ��
(defun fl_undo_end (/ cmdstat) 
	(setq cmdstat (getvar "cmdecho"))
	(setvar "cmdecho" 0)
	(cmd "undo" "end")
	(setvar "cmdecho" cmdstat)
)

;��ȡ���Xֵ
(defun fl_get_x (pt) (car pt))

;��ȡ���Yֵ
(defun fl_get_y (pt) (cadr pt))

;��ȡ���Zֵ�����û��Zֵ������
(defun fl_get_z (pt / zvalue) 
	(setq 
		zvalue (caddr pt)
		zvalue (if zvalue zvalue 0.0)
	)
)

;��ȡPline�����б�
(defun fl_get_plvtx (pl_ent / vtx_list itm) 
	(setq vtx_list (list))
	(foreach itm pl_ent 
		(if (= (car itm) 10) 
			(setq vtx_list (append vtx_list (list (cdr itm))))
		)
	)
	vtx_list
)

;��ȡһ��LIST��ÿ��N��Ԫ���γ�һ���±�
;�������ڳ�ȡһ����X�б���Y�б���
;������
;list_list	�ɱ���ɵı�������((1 10 100) (2 20 200) (3 30 300))
;n		��ȡλ�ã�0Ϊ��һλ
(defun fl_get_nth (list_list n) 
	(mapcar '(lambda (x) (nth n x)) list_list)
)

;��ѡ��ת��Ϊʵ���б�
(defun fl_ss2objlist (ss / objlist idx) 
	(setq 
		objlist (list)
		idx 0
	)
	(repeat (sslength ss) 
		(setq 
			objlist (append objlist (list (ssname ss idx)))
			idx (1+ idx)
		)
	) ;repeat
	objlist
)

;��ȡLineѡ�񼯵Ķ����б�
;������
; ssLineʵ��ѡ��
; code10�г���㣬11�г��յ㣬����ֵ�����ͬʱ�г�����
(defun fl_line2ptlist (ss code) 
	(cond 
		((member code (list 10 11))
		 (mapcar 
		 	'(lambda (x) (cdr (assoc code (entget x))))
			(fl_ss2objlist ss)
		 )
		)
		(t
		 (mapcar 
			'(lambda (x) (list (cdr (assoc 10 (entget x))) (cdr (assoc 11 (entget x)))))
			(fl_ss2objlist ss)
		 )
		)
	)
)

(defun fl_bench_begin () 
	(setq fl_bench_tm1 (getvar "date"))
)

(defun fl_bench_end () 
	(if fl_bench_tm1 
		(progn 
			(princ "\nTime: ")
			(setq fl_bench_tm2 (getvar "date"))
			(princ (* 86400.0 (- fl_bench_tm2 fl_bench_tm1)))
			(princ "s.")
			(setq fl_bench_tm2 nil fl_bench_tm1 nil)
		) ;progn
	) ;if
)

(defun fl_layer_check (lay_name lay_clr) 
	(if (tblsearch "Layer" lay_name) 
		nil
		(progn 
			(setvar "cmdecho" 0)
			(cmd "layer" "new" lay_name "color" lay_clr lay_name "")
			(setvar "cmdecho" 1)
		)
	)
)

(defun fl_var_set (var_name var_value)
	(append var_list (list (list var_name (getvar var_name))))
	(setvar var_name var_value)
)

(defun fl_var_res ()
	(mapcar
		'(lambda (var) (setvar (car var) (cadr var)))
		var_list
	)
	(setq var_list (list))
)

(defun c:fl_list ()
	(princ (strcat
		"\ncmd\tIf COMMAND-S available use COMMAND-S, otherwise COMMAND."
		"\nfl_get_nth\tMake new list with nth itiem of inner list in outer list."
		"\nfl_get_plvtx\tGet points list in PLINE."
		"\nfl_get_x"
		"\nfl_get_y"
		"\nfl_get_z"
		"\nfl_layer_check\tCheck layer, if not available, make it with certain color."
		"\nfl_make_modi_layer\tMake a layer named as MODIyymmdd."
		"\nfl_line2ptlist\tLINE entities selection to Points list."
		"\nfl_ss2objlist\tEntities selection to list."
		"\nfl_undo_begin"
		"\nfl_undo_end"
		"\nfl_var_res\tRestore system varable to old value saved by fl_var_set."
		"\nfl_var_set\tSet system varable and save old value."
	))
	(princ)
)

(defun fl_make_modi_layer (/ lname unplt)
	(setvar "cmdecho" 0)
	(setq lname (strcat "modi" (substr (rtos (getvar "cdate")) 3 6)))
	(setq unplt (not (tblsearch "layer" lname)))
	(cmd "_.layer" "m" lname "")
	(if unplt
		(cmd "_.layer" "c" "r" "" "plot" "n" "" "")
	)
	(setvar "cmdecho" 1)
)

(princ)