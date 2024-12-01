; 2006-12-31 v4.4	纠正了关于图块比例的一个概念性错误。
; 2005-11-15 v4.3	增加了引线去箭头的格式设置；
; 					去掉了标注块中两个一直没有用到的属性。
; 2005-11-07 v4.2	增加了对Units的判断，以确定标注值。
; 2005-05-09 v4.1	修正了一个比例错误。
; 2004-03-05 v4.0	增加直接在UCS中以WCS标注功能；
; 					根据DIMSCALE自动设置标注大小；
; 					使用Block和引线来标注。
; 1999-05-07 v3.0	UCS转换功能加强，可以在UCS中标注；
;					可以用任何角度标注；
;					解决一个UNDO-END错误。
; 1998-04-03 v2.0	增加比例控制。
; 1998-02-05 v1.1	增加UCS模式转换。
; 1998-01-26 V1.0

; maybe X,Y value exchange may be available;
; Unnamed block will be better to be modified;
; Find UCS by 2 points may be available.

(setq olderr *error*)
(defun *error* (s)
	(princ "加载失败！\n未找到 ZBL.DWG 或 ZBR.DWG。")
	(setq *error* olderr olderr nil)
	(princ)
)
(foreach item (list "zbl.dwg" "zbr.dwg")
	(if (findfile item) nil (exit))
)
(setq *error* odlerr olderr nil)

(defun zb_opt ()
	(princ (strcat
		"\nX前缀："
		(if (= zb_xpf ".") "<无>" (strcat "\"" zb_xpf "\""))
		"；Y前缀："
		(if (= zb_ypf ".") "<无>" (strcat "\"" zb_ypf "\""))
		"；精度："
		(rtos zb_pcs 2 0)
		"；使用"
		(if zb_wcs "WCS" "UCS")
	));princ
	(initget "F P CS")
	(setq pt0 (getpoint "\n选项：[F前缀/P精度/CS]<点取座标>: "))
	(cond
		((= pt0 "F")
			(princ (strcat "\nX前缀，或输入 . 取消：<" zb_xpf "> "))
			(setq xpf (getstring 1))
			(if (= xpf "") (setq xpf zb_xpf))
			(setq zb_xpf xpf)
			(princ (strcat "\nY前缀，或输入 . 取消：<" zb_ypf "> "))
			(setq ypf (getstring 1))
			(if (= ypf "") (setq ypf zb_ypf))
			(setq zb_ypf ypf)
			(zb_opt)
		)
		((= pt0 "P")
			(princ "\n精度：<")
			(princ zb_pcs)
			(setq pcs (getint "> "))
			(if pcs nil (setq pcs zb_pcs))
			(setq zb_pcs pcs)
			(zb_opt)
		)
		((= pt0 "CS")
			(setq zb_wcs (not zb_wcs))
			(zb_opt)
		)
	);cond
);zb_opt

(defun c:zb (/ cmdstat olderr sty wid oo pt0 pt0a pt1 pt2 pt3 pt4 pt5 x y dir sc hlfsc a90 a-90 ali tbox dis xpf ypf pcs thi insu v_fa)
	(setvar "cmdecho" 0)
	(setq cmdstat (getvar "cmdecho"))
	(setvar "cmdecho" 0)
	(if (fl_check_ver 12)
		(cmd "undo" "group")
		(cmd "undo" "begin")
	)
	(princ "test")
	(setvar "cmdecho" cmdstat)
	(setq olderr *error*)
	(defun *error* (s)
		(setvar "cmdecho" 0)
		(if (= s "Function cancelled") nil
		(princ (strcat "\n错误：" s))
		)
		(if oo (setvar "orthomode" oo))
		(setq *error* (if olderr olderr *pub_err*))
		(setq cmdstat (getvar "cmdecho"))
		(setvar "cmdecho" 0)
		cmd "undo" "end")
		(setvar "cmdecho" cmdstat)
		(setvar "cmdecho" 1)
		(princ)
	)
	(princ "\nFishLISP. C:ZB Version 4.4.i")
	(defun set_insunits (/ insu dwgu)
		(setq insu (getvar "insunits"))
		(cond
			((= insu 4);ms
				(setq v_fa 0.001)
			)
			((= insu 5);cm
				(setq v_fa 0.01)
			)
			((= insu 6);m
				(setq v_fa 1.0)
			)
			(t
				(initget "H M")
				(setq dwgu (getkword "\n指定本图单位：[H毫米/M米]"))
				(cond
					((= dwgu "H")
						(setvar "insunits" 4)
					)
					((= dwgu "M")
						(setvar "insunits" 6)
					)
					(t)
				);cond dwgu
				(set_insunits)
			)
		);cond
	)
	(set_insunits)

	(if zb_xpf nil (setq zb_xpf "X="))
	(if zb_ypf nil (setq zb_ypf "Y="))
	(if zb_pcs nil (setq zb_pcs 3))
	(setq oo (getvar "orthomode"))

	(zb_opt)
	(setvar "orthomode" 0)
	(if pt0
		(setq
			pt0a (if zb_wcs (trans pt0 1 0) pt0)
			x0 (car pt0a)
			x1 (rtos (* x0 v_fa) 2 zb_pcs)
			x1 (if (= zb_ypf ".") x1 (strcat zb_ypf x1))
			y0 (cadr pt0a)
			y1 (rtos (* y0 v_fa) 2 zb_pcs)
			y1 (if (= zb_xpf ".") y1 (strcat zb_xpf y1))
			pt1 (getpoint pt0 "\n标注点：")
		)
	)
	(setvar "orthomode" 1)
	(if pt1
		(progn
			(setq zb_sym (if (< (car pt0) (car pt1)) "zbr" "zbl"))
			(cmd "leader" pt0 pt1 "f" "n" "" "" "b" zb_sym "s" (getvar "dimscale") pt1 "" y1 x1)
		);progn
	)
	(if oo (setvar "orthomode" oo))
	(setq cmdstat (getvar "cmdecho"))
	(setvar "cmdecho" 0)
	(cmd "undo" "end")
	(setvar "cmdecho" cmdstat)
	(setvar "cmdecho" 1)
	(setq *error* olderr)
	(princ)
)

(princ "loaded.\nStart as C:ZB ")
(princ)
