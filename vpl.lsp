; FishLISP C:VPL
; -----------------------------------------------
; ����/����ViewPort(s)
; ViewPortһ����������Mspace��Pspace��ͬ����ʾ����
; ��ֹ��Layout��Mspace����������š�ƽ��ͼ�Ρ�
; -----------------------------------------------
; ViewPortʵ�������������Ϣ��¼������90������һ��
; bit-coded flags������״̬Ϊ16384 (0x4000)
; 
; ������Ϊ������������ÿ���ģʽ��������ÿһ��ViewPort
; ��������������ͬʱ����/�������е�ViewPort�����ֻ
; ��Ҫ��õ�һ��ViewPort������״̬���ɡ�
;
; �������������ʹ����ͬһ������C:VPL��
; ����֮��ֱ�ӵõ�������Ϣ����������������
; -----------------------------------------------
; 2004-07-07v1.1 ���TILEMODE=1����
; 2004-07-05v1.0 ��������
; -----------------------------------------------

(defun c:vpl (/ vp_ss vp_ob vp_lo cmdflg msg)
	(princ "\nFishLISP C:VPL v1.1.")
	(if (zerop (getvar "tilemode"))
		(if (setq vp_ss (ssget "x" '((0 . "VIEWPORT"))))
		(progn
			(setq 
				vp_ob (entget (ssname vp_ss 0))
				vp_lo (if (= (logand 16384 (cdr (assoc 90 vp_ob))) 16384) t)
			)
			(if vp_lo
				(setq cmdflg "off" msg "\nViewPort ���������ѱ��⿪��")
				(setq cmdflg "on" msg "\nViewPort �����ѱ�������")
			)
			(setvar "cmdecho" 0)
			(cmd "mview" "l" cmdflg "all" "")
			(setvar "cmdecho" 1)
			(princ msg)
		);progn vp_ss exist
		(princ "\nû�� ViewPort ʵ�塣")
		);TILEMODE=0
		(princ "\n����������� Layout��"); TILEMODE=1
	)
	(princ)
)

(princ "loaded. Start as C:VPL")
(princ)