; 总图标高、坡度工具集
; 2025-11-19		增加 rotate_labels，处理文本颠倒的情况
; 2025-11-17		重写结构
;					增加了vla函数求曲线中点及中点切线角度，用于放置标注
;					今天才知道，(atof "any") 返回 0.0，所以处理 %%P0.000 其实非常简单
; 2023-10-08 v1.4	C:GR 增加了多段线距离，未测试
; 2014-02-26 v1.3	修改一个小错误
;		 			增加了 insunitsdefsource 归零设置
; 2005-03-23 v1.2	根据宋的建议，修改坡度标注方式
; 2004-08-11 v1.1	修改几个错误
; 2004-07-16 v1.0	存在%%P0.000的错误
;					存在图块插入不匹配的错误
; 2004-06-23 v1.0	正式版
;					改进图层控制，修改一些错误。
; 2004-04-24 v0.1	原始版本
;			C:GR	根据两个总平面标高，计算并标注这两点之间的坡度和距离；
;			C:GD	以一个平面标高为基准点，标注一定坡度的另一点标高；
;			C:GM	根据两个标高，标注同一坡度上另一点标高。
;			C:GX	根据两个标高，求同一坡度上指定标高的位置，并标注。

(vl-load-com)

(defun set_insunits (/ insu dwgu)
	(setvar "insunitsdefsource" 0)
	(setvar "dimzin" 0)
	(setq insu (getvar "insunits"))
	(cond
		((= insu 4);毫米
			(setq VP_FAC 0.001)
		)
		((= insu 5);厘米
			(setq VP_FAC 0.01)
		)
		((= insu 6);米
			(setq VP_FAC 1.0)
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

(defun get_curve_len (en_name_ / obj)
	(setq obj (vlax-ename->vla-object en_name_)) ; 转成 VLA 对象
	(vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj))
)

; 求曲线中点, ok
(defun get_curve_midpt (en_name_ / obj len)
	(setq obj (vlax-ename->vla-object en_name_)) ; 转成 VLA 对象
	(setq len (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj))) ; 曲线总长
	(vlax-curve-getPointAtDist obj (/ len 2.0))
)

; 求曲线中点处切线方向, ok
(defun get_curve_midang (en_name_ / obj len deriv)
	(setq obj (vlax-ename->vla-object en_name_)) ; 转成 VLA 对象
	(setq len (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj))) ; 曲线总长
	(setq deriv (vlax-curve-getFirstDeriv obj (vlax-curve-getParamAtDist obj (/ len 2.0)))) ; 一阶导数向量
	(angle '(0.0 0.0 0.0) deriv) ; 向量与 X 轴夹角
)

; ok
(defun place_label (pt_ ang_ lay_ label1_ label2_ / dimsc dim_g dim_h pt_ofs dimpt1 dimpt2)
	(setq
		dimsc (getvar "dimscale")
		dim_g (* (getvar "dimgap") dimsc)
		dim_h (* (getvar "dimtxt") dimsc)
		pt_ofs (+ dim_g (/ dim_h 2))
		dimpt1 (polar pt_ (+ ang_ (/ pi 2)) pt_ofs)
		dimpt2 (polar pt_ (- ang_ (/ pi 2)) pt_ofs)
		label_ss (ssadd)
	)
	(entmake (list '(0 . "text") (cons 1 label1_) (cons 62 7) (cons 72 4) (cons 8 lay_) (cons 10 dimpt1) (cons 11 dimpt1) (cons 40 dim_h) (cons 50 ang_)))
	(setq label_ss (ssadd (entlast) label_ss))
	(entmake (list '(0 . "text") (cons 1 label2_) (cons 62 7) (cons 72 4) (cons 8 lay_) (cons 10 dimpt1) (cons 11 dimpt2) (cons 40 dim_h) (cons 50 ang_)))
	(setq label_ss (ssadd (entlast) label_ss))
	(rotate_labels label_ss pt_)
)

(defun rotate_labels (ss_ pt_ / ot)
	(setq ot (getvar "orthomode"))
	(setvar "orthomode" 1)
	(setvar "cmdecho" 0)
	(cmd "_rotate" ss_ "" pt_ pause)
	(setvar "cmdecho" 1)
	(setvar "orthomode" ot)
)

; ok
(defun get_elevation (en_name_ / attr_txt)
	(atof (cdr (assoc 1 (entget (entnext en_name_)))))
)

(defun get_ins (en_name_)
	(cdr (assoc 10 (entget en_name_)))
)

(defun make_el_ins (bkname_ pt_ attr_ / os lay)
	(if (= attr_ "0.000") (setq attr_ "%%P0.000"))
	(setq os (getvar "osmode"))
	(setq lay (getvar "clayer"))
	(setvar "osmode" 0)
	(setvar "cmdecho" 0)
	(setvar "clayer" bk_lay)
	(cmd "_insert" bkname_ pt_ (getvar "dimscale") "" 0 attr_)
	(setvar "clayer" lay)
	(setvar "osmode" os)
	(setvar "cmdecho" 1)
)

(defun c:gr (/ bk_ss bk_en1 bk_en2 bk_in1 bk_in2 bk_lay dis ang mid_pt at_en1 at_en2 att1 att2 hig gra gra_t dis_t)
	(set_insunits)
	(setq bk_ss (ssget '((0 . "insert") (2 . "elur,elul,eldr,eldl,dim_bg,dim_rl"))))
	(if (and bk_ss (< (sslength bk_ss) 2))
		(setq bk_ss nil)
	)
	(if bk_ss
		(setq pl_en0 (entsel "\n选取两点间多段线<回车取直线距离>："))
		(princ "\n未选中标高图块")
	)
	(if pl_en0
		(setq
			en_crv (car pl_en0)
			dis (get_curve_len en_crv)
			mid_pt (get_curve_midpt en_crv)
			ang (get_curve_midang en_crv)
		)
		(setq dis nil mid_pt nil ang nil)
	)
	(if bk_ss
		(progn
			(setq
				bk_en1 (ssname bk_ss 0)
				bk_en2 (ssname bk_ss 1)
				bk_in1 (cdr (assoc 10 (entget bk_en1)))
				bk_in2 (cdr (assoc 10 (entget bk_en2)))
				bk_lay (cdr (assoc 8 (entget bk_en1)))
				dis (if dis dis (distance bk_in1 bk_in2))
				ang (if ang ang (angle bk_in1 bk_in2))
				mid_pt (if mid_pt mid_pt (polar bk_in1 ang (/ dis 2)))
				at_en1 (entnext bk_en1)
				at_en2 (entnext bk_en2)
				att1 (get_elevation bk_en1)
				att2 (get_elevation bk_en2)
				hig (abs (- att1 att2))
				gra (/ (* hig 100) dis VP_FAC)
				gra_t (strcat (rtos gra 2 1) "%")
				dis_t (rtos (* dis VP_FAC) 2 2)
			)
			(place_label mid_pt ang bk_lay gra_t dis_t)
		)
	)
	(princ)
)

(defun c:gd (/ en pt0 a0 pt1 podu attr tmpval bkname bk_lay)
	(set_insunits)
	(setq en (entsel "\n选取基准标高："))
	(if en
		(setq en (car en))
	)
	(if (and
			(= (cdr (assoc 0 (setq ent (entget en)))) "INSERT")
			(member (strcase (cdr (assoc 2 (entget en)))) (list "DIM_RL" "DIM_BG"))
		)
		(progn
			(setq
				pt0	(trans (cdr (assoc 10 ent)) 0 1)
				bkname (cdr (assoc 2 ent))
				bk_lay (cdr (assoc 8 ent))
				a0	(atof (cdr (assoc 1 (entget (entnext en)))))
			)
			(setq a0 (if a0 a0 0.0))
			(setq pt1 (getpoint pt0 "\n目标点："))
			(setq podu (getreal "\n坡度(%)："))
			(setq attr (rtos (+ a0 (* podu (distance pt0 pt1) 0.01 VP_FAC)) 2 3))
			(setq tmpval (getstring (strcat "\n确认标高：<" attr ">: ")))
			(if (= tmpval "") nil (setq attr tmpval))
			(make_el_ins bkname pt1 attr)
		)
		(princ "\n所选非标高块。")
	)
   
	(princ)
);defun

(defun c:gm (/ bk_ss bk_en1 bk_en2 bk_in1 bk_in2 bk_lay at_en1 at_en2 h1 h2 hd dis0 newpt dis1 attr)
	(set_insunits)
	(setq bk_ss (ssget '((0 . "insert") (2 . "dim_bg,dim_rl"))))
	(if (< (sslength bk_ss) 2) (setq bk_ss nil))
	(if (not bk_ss) (exit))
  
	(setq 
		bk_en1 (ssname bk_ss 0)
		bk_en2 (ssname bk_ss 1)
		bk_in1 (trans (cdr (assoc 10 (entget bk_en1))) 0 1)
		bk_in2 (trans (cdr (assoc 10 (entget bk_en2))) 0 1)
		bk_lay (cdr (assoc 8 (entget bk_en1)))
		bk_nam (cdr (assoc 2 (entget bk_en1)))
		at_en1 (entnext bk_en1)
		at_en2 (entnext bk_en2)
		h1 (get_elevation bk_en1)
		h2 (get_elevation bk_en2)
		hd (- h2 h1)
		dis0 (distance bk_in1 bk_in2)
		newpt (getpoint "\n目标点：")
		dis1 (distance bk_in1 newpt)
		attr (rtos (+ h1 (* (/ hd dis0) dis1)) 2 3)
	)
	(make_el_ins bk_nam newpt attr)
	(princ)
)

(defun c:gx (/ bk_ss bk_en1 bk_en2 bk_in1 bk_in2 bk_lay at_en1 at_en2 h1 h2 hd dis0 newpt newhd newdis dis1 attr)
	(set_insunits)
	(setq bk_ss (ssget '((0 . "insert") (2 . "dim_bg,dim_rl"))))
	(if (< (sslength bk_ss) 2) (setq bk_ss nil))
	(if (not bk_ss) (exit))
	(setq 
		bk_en1 (ssname bk_ss 0)
		bk_en2 (ssname bk_ss 1)
		bk_in1 (trans (cdr (assoc 10 (entget bk_en1))) 0 1)
		bk_in2 (trans (cdr (assoc 10 (entget bk_en2))) 0 1)
		bk_lay (cdr (assoc 8 (entget bk_en1)))
		bk_nam (cdr (assoc 2 (entget bk_en1)))
		at_en1 (entnext bk_en1)
		at_en2 (entnext bk_en2)
		h1 (get_elevation bk_en1)
		h2 (get_elevation bk_en2)
		hd (- h2 h1)
		dis0 (distance bk_in1 bk_in2)
		ang0 (angle bk_in1 bk_in2)
		newhd (getreal "\n指定标高值：")
		newdis (* (/ (- newhd h1) hd) dis0)
		newpt (polar bk_in1 ang0 newdis)
		attr (rtos newhd 2 3)
	)
	(make_el_ins bk_nam newpt attr)
	(princ)
)

(princ "\nloaded. start as C:GR C:GD C:GM C:GX")
(princ)