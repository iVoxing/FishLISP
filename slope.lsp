; �����¶Ȼ�������
; ͨ����ֹ������Ƿ���������¶����ƣ�����������¶Ȼ�����¶ȵĹؼ�����

(defun c:fslope (/ osm pt1 pt2 h0 b0 slp1 b_min h1)
	;; ��������
	(setq osm (getvar "osmode"))
	;; ��ȡ��ֹ��
	(setq pt1 (getpoint "\n��㣺"))
	(if pt1 (setq pt2 (getpoint pt1 "\n�յ�")))
	(if pt2
		(if (> (car pt1) (car pt2))
			(setq temp pt1 pt1 pt2 pt2 temp temp nil)
		)
	)
	;; ���ݸ߲������С�³�����ȷ����ֹ���Ƿ���������¶�����
	(setq 
		h0 (- (cadr pt2) (cadr pt1))
		b0 (- (car pt2) (car pt1))
	)
	(setq slp1 (getreal "\n����ˮƽ����<3600>��"))
	(setq slp1 (if slp1 slp1 3600.0))
	(setq b_min (+ (* slp1 2) (/ (- h0 (* slp1 2 0.075)) 0.15)))
	;; 
	(setvar "osmode" 0)
	(cond
		((< b0 b_min) ; ����������
			(alert "��ֹ�㲻��������¶�����\n��������¶���Ϊ�ο�")
			(setq 
				spt1 pt2
				spt2 (list (- (car pt2) slp1) (- (cadr pt2) (* slp1 0.075)))
				spt3 (list (- (car spt2) (/ (- h0 (* slp1 0.15)) 0.15)) (+ (cadr pt1) (* slp1 0.075)))
				spt4 (list (- (car spt3) slp1) (cadr pt1))
			)
		);< b0 b_min
		(t
			(princ "\n��ֹ����������¶����ơ�")
			(setq 
				h1 (/ (* (/ h0 2) slp1) (- b0 slp1))
				spt1 pt2
				spt2 (list (- (car pt2) slp1) (- (cadr pt2) h1))
				spt3 (list (+ (car pt1) slp1) (+ (cadr pt1) h1))
				spt4 pt1
			)
		);>= b0 b_min
	);cond
	(setvar "cmdecho" 0)
	(cmd "line" spt1 spt2 spt3 spt4 "")
	(setvar "cmdecho" 1)
	;; �ָ���������
	(setvar "osmode" (if osm osm 175))
);defun

(princ " Start as C:FSLOPE")