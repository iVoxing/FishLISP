; FishLISP C:VPL
; -----------------------------------------------
; 锁定/解锁ViewPort(s)
; ViewPort一旦被锁定，Mspace和Pspace即同步显示，可
; 防止在Layout的Mspace中意外地缩放、平移图形。
; -----------------------------------------------
; ViewPort实体的缩放锁定信息记录于组码90，这是一个
; bit-coded flags。锁定状态为16384 (0x4000)
; 
; 本程序为方便起见，采用开关模式，并不对每一个ViewPort
; 单独操作，而是同时锁定/解锁所有的ViewPort，因此只
; 需要求得第一个ViewPort的锁定状态即可。
;
; 锁定或解锁，都使用相同一个命令C:VPL。
; 运行之后直接得到反馈信息，无需其他操作。
; -----------------------------------------------
; 2004-07-07v1.1 解决TILEMODE=1错误
; 2004-07-05v1.0 正常工作
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
				(setq cmdflg "off" msg "\nViewPort 缩放锁定已被解开。")
				(setq cmdflg "on" msg "\nViewPort 缩放已被锁定。")
			)
			(setvar "cmdecho" 0)
			(cmd "_mview" "l" cmdflg "all" "")
			(setvar "cmdecho" 1)
			(princ msg)
		)
		(princ "\n没有 ViewPort 实体。")
		)
		(princ "\n本命令仅用于 L)EMODE=1
	)
	(princ)
)

(princ "loaded. Start as C:VPL")
(princ)