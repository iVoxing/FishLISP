; FishLISP C:RNB
; ͨ�����ʵ�壬����ͼ�����ơ������ѯ������ԭ�����ơ�
;
; ��AutoCAD���У����Ժܷ���ظ���һ��ʵ�壬��ճ����ͼ�顣
; �����õ���ͼ�����Ǿ����硰A$C4FD24C92��֮�ิ�ӵ����ƣ�
; ������������ʶ��Ͳ�����������ʱ���б�Ҫ��������ͼ���
; �����������Ƶĸ����ԣ�ͬ��ʹ�ø���Ҳ�������ס�
; C:RNB������˲�ѯ������ͼ�����ƣ�ʹ������Ϊ������
;
; ����֧�ֳ����ƣ�����������������ո񣩣����������Ʊ���
; ���س���ȷ�ϡ�
;-----------------------------------------------------
; 2004-07-07v1.0���ð�
;-----------------------------------------------------

(defun c:rnb (/ en ent ety ena enn)
	(princ "\nFishLISP C:RNB v1.0.")
	(if (setq en (entsel "\nѡȡҪ������ͼ�飺"))
		(if en
			(progn
				(setq 
					ent (entget (car en))
					ety (cdr (assoc 0 ent))
				)
				(if (= ety "INSERT")
					(progn
						(setq ena (cdr (assoc 2 ent)))
						(setq loop t)
						(while loop
							(princ "\nͼ�����ƣ�<")
							(princ ena)
							(setq enn (getstring 1 "> "))
							(if (member enn (list "" " " ena))
								(setq loop nil enn ena)
								(if (tblsearch "block" enn)
									(princ "\n�Ѿ����ڴ����Ƶ�ͼ�顣")
									(setq loop nil)
								); if exist?
							); if no change?
						); while
						(setvar "cmdecho" 0)
						(cmd "rename" "block" ena enn)
						(setvar "cmdecho" 1)
					); progn =INSERT
					(princ "\n������ͼ�顣")
				);if INSERT?
			); progn en
		); if en?
	); if picked
	(princ)
)

(princ)

