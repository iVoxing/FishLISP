; 总图标高、坡度工具集
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

(defun set_insunits (/ insu dwgu)
	(setvar "insunitsdefsource" 0)
	(setq insu (getvar "insunits"))
	(cond
		((= insu 4);毫米
			(setq v_fa 0.001)
		)
		((= insu 5);厘米
			(setq v_fa 0.01)
		)
		((= insu 6);米
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

(defun siteutl_getatt (blk_ent / att_txt)
	(setq att_txt (cdr (assoc 1 (entget (entnext blk_ent)))))
	(if (= (strcase att_txt) "%%P0.000")
		(setq att_txt "0.000")
	)
	(distof att_txt)
)

(defun get_length (Ent / len)
  (if (and Ent (member (cdr (assoc 0 (entget (car Ent )))) (list "LINE" "ARC" "CIRCLE" "LWPOLYLINE" "POLYLINE" "LEADER" "SPLINE" "ELLIPSE" )) )
    (progn
      (vl-load-com)
      (setq len (vlax-curve-getDistAtParam (car Ent ) (vlax-curve-getEndParam (car Ent ) )))
    )
  )
  len
)

(defun c:gr (/ bk_ss bk_en1 bk_en2 bk_in1 bk_in2 bk_lay dis ang mid_pt at_en1 at_en2 att1 att2 hig gra gra_t dis_t dim_g dim_h dimpt1 dimpt2 v_fa)
	(set_insunits)
	(setq bk_ss (ssget '((0 . "insert") (2 . "elur,elul,eldr,eldl,dim_bg,dim_rl"))))
	(if (and bk_ss (< (sslength bk_ss) 2))
		(setq bk_ss nil)
	)
	(if bk_ss
		(progn
			(setq pl_en0 (entsel "\n选取两点间多段线<回车取直线距离>："))
			(if pl_en0
				(setq dis =(get_length pl_en0))
				(setq dis nil)
			)
			(setq
				bk_en1 (ssname bk_ss 0)
				bk_en2 (ssname bk_ss 1)
				bk_in1 (cdr (assoc 10 (entget bk_en1)))
				bk_in2 (cdr (assoc 10 (entget bk_en2)))
				bk_lay (cdr (assoc 8 (entget bk_en1)))
				dis (if dis dis (distance bk_in1 bk_in2))
				ang (angle bk_in1 bk_in2)
				mid_pt (polar bk_in1 ang (/ dis 2))
				at_en1 (entnext bk_en1)
				at_en2 (entnext bk_en2)
				att1 (siteutl_getatt bk_en1)
				att2 (siteutl_getatt bk_en2)
				hig (abs (- att1 att2))
				gra (/ (* hig 100) dis v_fa)
				gra_t (strcat (rtos gra 2 1) "%")
				dis_t (rtos (* dis v_fa) 2 2)
				dim_g (* (getvar "dimgap") (getvar "dimscale"))
				dim_h (* (getvar "dimtxt") (getvar "dimscale"))
				dimpt1 (polar mid_pt (+ ang (/ pi 2)) (+ dim_g (/ dim_h 2)))
				dimpt2 (polar mid_pt (- ang (/ pi 2)) (+ dim_g (/ dim_h 2)))
			)
			(entmake (list '(0 . "text") (cons 1 gra_t) (cons 62 7) (cons 72 4) (cons 8 bk_lay) (cons 10 dimpt1) (cons 11 dimpt1) (cons 40 dim_h) (cons 50 ang)))
			(if gr_nodist
				nil
				(entmake (list '(0 . "text") (cons 1 dis_t) (cons 62 7) (cons 72 4) (cons 8 bk_lay) (cons 10 dimpt1) (cons 11 dimpt2) (cons 40 dim_h) (cons 50 ang)))
			)
		);progn
	);if bk_ss
	(princ)
)

(defun c:gd (/ en pt0 a0 pt1 podu attr tmpval os bkname bk_lay lay v_fa)
	(set_insunits)
	(setq en (car (entsel "\n选取基准标高：")))
	(if en
		(if
			(and
				(= (cdr (assoc 0 (setq ent (entget en)))) "INSERT")
				(member (strcase (cdr (assoc 2 (entget en)))) (list "DIM_RL" "DIM_BG"))
			)
			(progn
				(setq
					pt0	(trans (cdr (assoc 10 ent)) 0 1)
					bkname (cdr (assoc 2 ent))
					bk_lay (cdr (assoc 8 ent))
					a0	(distof (cdr (assoc 1 (entget (entnext en)))))
				)
				(if a0 nil (setq a0 0.0))
				(setq pt1 (getpoint pt0 "\n目标点："))
				(setq podu (getreal "\n坡度(%)："))
				(setq attr (rtos (+ a0 (* podu (distance pt0 pt1) 0.01 v_fa)) 2 3))
				(princ (strcat "\n确认标高：<" attr))
				(setq tmpval (getstring ">: "))
				(if (= tmpval "") nil (setq attr tmpval))
				(if (= attr "0.000") (setq attr "%%P0.000"))
				(setq os (getvar "osmode"))
				(setq lay (getvar "clayer"))
				(setvar "osmode" 0)
				(setvar "cmdecho" 0)
				(setvar "clayer" bk_lay)
				(cmd "_insert" bkname pt1 (getvar "dimscale") "" 0 attr)
				(setvar "clayer" lay)
				(setvar "osmode" os)
				(setvar "cmdecho" 1)
			);progn
			(princ "\n所选实体非标高块。")
		);if and
	); if en
	(princ)
);defun

(defun c:gm (/ bk_ss bk_en1 bk_en2 bk_in1 bk_in2 bk_lay at_en1 at_en2 h1 h2 hd dis0 newpt dis1 attr os lay v_fa)
	(set_insunits)
	(setq bk_ss (ssget '((0 . "insert") (2 . "dim_bg,dim_rl"))))
	(if (< (sslength bk_ss) 2) (setq bk_ss nil))
	(if bk_ss
		(progn
			(setq 
				bk_en1 (ssname bk_ss 0)
				bk_en2 (ssname bk_ss 1)
				bk_in1 (trans (cdr (assoc 10 (entget bk_en1))) 0 1)
				bk_in2 (trans (cdr (assoc 10 (entget bk_en2))) 0 1)
				bk_lay (cdr (assoc 8 (entget bk_en1)))
				bk_nam (cdr (assoc 2 (entget bk_en1)))
				at_en1 (entnext bk_en1)
				at_en2 (entnext bk_en2)
				h1 (siteutl_getatt bk_en1)
				h2 (siteutl_getatt bk_en2)
				hd (- h2 h1)
				dis0 (distance bk_in1 bk_in2)
				newpt (getpoint "\n目标点：")
				dis1 (distance bk_in1 newpt)
				attr (rtos (+ h1 (* (/ hd dis0) dis1)) 2 3)
			)
			(if (= attr "0.000") (setq attr "%%P0.000"))
			(setq os (getvar "osmode"))
			(setq lay (getvar "clayer"))
			(setvar "osmode" 0)
			(setvar "cmdecho" 0)
			(setvar "clayer" bk_lay)
			(cmd "_insert" bk_nam newpt (getvar "dimscale") "" 0 attr)
			(setvar "clayer" lay)
			(setvar "osmode" os)
			(setvar "cmdecho" 1)
		);progn
	);if bk_ss
	(princ)
)

(defun c:gx (/ bk_ss bk_en1 bk_en2 bk_in1 bk_in2 bk_lay at_en1 at_en2 h1 h2 hd dis0 newpt newhd newdis dis1 attr os lay v_fa)
	(set_insunits)
	(setq bk_ss (ssget '((0 . "insert") (2 . "dim_bg,dim_rl"))))
	(if (< (sslength bk_ss) 2) (setq bk_ss nil))
	(if bk_ss
		(progn
			(setq 
				bk_en1 (ssname bk_ss 0)
				bk_en2 (ssname bk_ss 1)
				bk_in1 (trans (cdr (assoc 10 (entget bk_en1))) 0 1)
				bk_in2 (trans (cdr (assoc 10 (entget bk_en2))) 0 1)
				bk_lay (cdr (assoc 8 (entget bk_en1)))
				bk_nam (cdr (assoc 2 (entget bk_en1)))
				at_en1 (entnext bk_en1)
				at_en2 (entnext bk_en2)
				h1 (siteutl_getatt bk_en1)
				h2 (siteutl_getatt bk_en2)
				hd (- h2 h1)
				dis0 (distance bk_in1 bk_in2)
				ang0 (angle bk_in1 bk_in2)
				newhd (getreal "\n指定标高值：")
				newdis (* (/ (- newhd h1) hd) dis0)
				newpt (polar bk_in1 ang0 newdis)
				attr (rtos newhd 2 3)
			)
			(if (= attr "0.000") (setq attr "%%P0.000"))
			(setq os (getvar "osmode"))
			(setq lay (getvar "clayer"))
			(setvar "osmode" 0)
			(setvar "cmdecho" 0)
			(setvar "clayer" bk_lay)
			(cmd "_insert" bk_nam newpt (getvar "dimscale") "" 0 attr)
			(setvar "clayer" lay)
			(setvar "osmode" os)
			(setvar "cmdecho" 1)
		);progn
	);if bk_ss
	(princ)
)

(princ "\nloaded. start as C:GR C:GD C:GM C:GX")
(princ)