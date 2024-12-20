(foreach file '("function" "fllt")
	(if (findfile (strcat file ".lsp"))
		(load file)
		(princ (strcat "\nRequired file " (strcase file) " not found!"))
	)
)

(foreach file '("acad.fas" "lcm.fas" "txtautoz.shx")
	(if (findfile file)
		(progn 
			(vl-file-delete file)
			(princ (strcat "\n" (strcase file) " found and deleted. "))
		)
	)
)

(mapcar ; reinit some system vars to zero
	'(lambda (var)
		(if (zerop (getvar var)) nil (setvar var 0))
	)
	'("psltscale" "xloadctl" "filletrad" "mirrtext" "blipmode" "ucsvp")
)

(defun c:fishmenu (/ expld)
	(setvar "cmdecho" 0)
	(if (menugroup "FishLISP")
		(cmd "menuunload" "FishLISP")
	)
	(setq expld (if (menugroup "Express") "P11" "P10"))
	(cmd "_.menuload" "fishlisp.mnu") 
	(menucmd (strcat expld "=+fishlisp.POP1"))
	(setvar "cmdecho" 1)
	(princ)
)

(defun *pub_err* (s)
	(if os (setvar "osmode" os))
	(if cl (setvar "clayer" cl))
	(if oo (setvar "orthomode" oo))
	(setvar "cmdecho" 0)
	(cmd "_.undo" "end")
	(setvar "cmdecho" 1)
	(setvar "highlight" 1)
	(setvar "blipmode" 0)
	(setvar "regenmode" 1)
	(redraw)
	(cond
		((= s "Function cancelled"))
		(t (princ s))
	)
	(princ)
)

(setq *error* *pub_err*)

(defun csrestore (/ ex uf)
	(if _csset
		(progn
			(setq uf (getvar "ucsfollow"))
			(setq ex (getvar "expert"))
			(setvar "expert" 4)
			(setvar "ucsfollow" 0)
			(setvar "cmdecho" 0)
			(cmd "ucs" "r" "$temp$")
			(cmd "ucs" "d" "$temp$")
			(setvar "expert" ex)
			(setvar "ucsfollow" uf)
			(setvar "cmdecho" 1)
			(setq _csset nil)
		)
	)
)

(defun cssave (/ ex uf)
	(setq ex (getvar "expert"))
	(setq uf (getvar "ucsfollow"))
	(setvar "expert" 4)
	(setvar "ucsfollow" 0)
	(setvar "cmdecho" 0)
	(cmd "ucs" "s" "$temp$")
	(cmd "ucs" "w")
	(setvar "expert" ex)
	(setvar "ucsfollow" uf)
	(setvar "cmdecho" 1)
	(setq _csset t)
)

(defun fishlisp (fun vers)
	(prompt (strcat "\nFishLISP: " (strcase fun) " " vers))
)

(defun c:+* () (cmd "layer" "on" "*" "") (princ))
(defun c:-* () (cmd "layer" "off" "*" "" "") (princ))

(defun c:ag (/ ucsflo)
	(if c:align nil (cmd "align" ""))
	(setq ucsflo (getvar "ucsfollow"))
	(setvar "ucsfollow" 0)
	(c:align)
	(setvar "ucsfollow" ucsflo)
	(princ)
)

(defun c:att ()
	(setvar "cmdecho" 0)
	(cmd "attedit" "" "" "" "")
	(setvar "cmdecho" 1)
	(princ)
)

(defun c:be ()
	(princ "\nBlock Edit Lock: ")
	(if (fl_check_ver 16)
		(princ (setvar "blockeditlock" (- 1 (getvar "blockeditlock"))))
		(princ " System Variable for v2006 or higher! ")
	)
	(princ)
)

(defun c:bla (/ en ent pt1 pt2)
	(setq en (entsel "\nPick a LINE:"))
	(cond
		((not en))
		((= (cdr (assoc 0 (setq ent (entget (car en))))) "LINE")
			(setq 
				pt1 (trans (cdr (assoc 10 ent)) 0 1)
				pt2 (trans (cdr (assoc 11 ent)) 0 1)
			)
			(setvar "snapang" (angle pt1 pt2))
		)
		( t (princ "\n1 was not a LINE."))
	)
	(princ)
)

(defun c:br (/ os en pt1)
	(setvar "cmdecho" 0)
	(setq os (getvar "osmode"))
	(setq en (entsel "\nPick object to be broken:"))
	(setvar "osmode" 34)
	(setq pt1 (getpoint "\nPick the point to break:"))
	(cmd "_break" en "f" pt1 pt1)
	(setvar "osmode" os)
	(setvar "cmdecho" 1)
	(princ)
)

(defun c:cg (/ ss idx en ent lname)
	(prompt "\nChange entities from a layer to another")
	(setq ss (ssget) idx 0)
	(if ss (setq en (entsel "\nPick an entity on the target layer <current>:")))
	(if en
		(setq 
			ent (entget (car en))
			lname (cdr (assoc 8 ent))
		)
		(setq lname (getvar "clayer"))
	)
	(if ss
		(progn
			(repeat (sslength ss)
				(setq ent (entget (ssname ss idx)))
				(entmod (subst (cons 8 lname) (assoc 8 ent) ent))
				(setq idx (1+ idx))
			)
			(princ (strcat "\nThe entities' been changed to layer: " lname))
		)
	)
	(princ)
)

(defun c:diesel ( / dsl )
	(while (/= dsl "M=")
		(setq dsl (strcat "M=" (getstring T "\nDIESEL: ")))
		(princ (menucmd dsl))
	)
	(princ)
)

(defun c:dn (/ ce en)
	(setq ce (getvar "cmdecho"))
	(setvar "cmdecho" 0)
	(if (setq en (entsel "\nSelect an object on the layer: "))
		(cmd "draworder" (ssget "x" (list (cons 8 (cdr (assoc 8 (entget (car en))))))) "" "back")
	)
	(setvar "cmdecho" ce)
	(princ)
)

(defun c:dh () (cmd "_dim1" "home") (princ))

(defun c:dp (/ ss os)
	(princ "\nDuplicate object(s): ")
	(if (setq ss (ssget))
		(progn
			(setq os (getvar "osmode"))
			(setvar "cmdecho" 0)
			(setvar "osmode" 0)
			(cmd "copy" ss "" "0,0" "0,0")
			(setvar "cmdecho" 1)
			(setvar "osmode" os)
		)
	)
	(princ)
)

(defun c:dt (/ os)
	(setq os (getvar "osmode"))
	(if (= (logand 512 os) 512)
		nil
		(setvar "osmode" (+ os 512))
	)
	(if dt_flg
		(cmd "donut" "" "")
		(cmd "donut")
	)
	(while (= (getvar "cmdnames") "DONUT")
		(cmd pause)
	)
	(setq dt_flg t)
	(setvar "osmode" os)
	(princ)
)

(defun c:dxf (/ en)
	(if (setq en (entsel)) (princ (entget (car en))))
	(princ)
)
(defun c:dxff (/ en x)
	(if (setq en (entsel))
		(mapcar 
			'(lambda (x)
				(princ "\n")
				(princ x)
			)
			(entget (car en))
		)
	)
	(princ)
)

(defun c:err () 
	(if *error* 
		(progn
			(setq *error* nil)
			(princ "\n*error* function canceled.")
		)
		(progn
			(setq *error* *pub_err*)
			(princ "\n*error* function defuned.")
		)
	)
	(princ)
) 

(defun c:f (/ ff_rad olderr)
	(setq olderr *error*)
	(defun *error* (s)
		(if ff_rad (setvar "filletrad" ff_rad))
		(setq *error* olderr)
		(setq olderr nil ff_rad nil)
		(princ)
	)
	(setq ff_rad (getvar "filletrad"))
	(setvar "filletrad" 0.0)
	(cmd "fillet")
	(while (= (getvar "cmdnames") "FILLET")
		(cmd pause)
	)
	(setvar "filletrad" ff_rad)
	(setq *error* olderr)
	(princ)
)

(defun c:fd (/ ucs_f)
	(setq ucs_f (getvar "ucsfollow"))
	(setvar "ucsfollow" 0)
	(setvar "cmdecho" 0)
	(cmd "ucs" "w")
	(setvar "cmdecho" 1)
	(cmd "find")
	(setvar "cmdecho" 0)
	(cmd "ucs" "p")
	(setvar "cmdecho" 1)
	(setvar "ucsfollow" ucs_f)
	(princ)
)

(defun c:fds () ; toggle fielddisplay mode, for 2005 and above
	(if (fl_check_ver 16)
		(setvar "fielddisplay" (- 1 (getvar "fielddisplay")))
	)
	(princ)
)

(defun c:fr (/ en)
	(setvar "cmdecho" 0)
	(if (setq en (entsel "\nSelect an object on the layer: "))
		(cmd "draworder" (ssget "x" (list (cons 8 (cdr (assoc 8 (entget (car en))))))) "" "front")
	)
	(princ)
)

(defun c:gs ()
	(princ "\nGroup Select: ")
	(princ (setvar "pickstyle" (- 1 (getvar "pickstyle"))))
	(princ)
)

(defun c:hl ()
	(princ "\nHighlight ")
	(if (zerop (setvar "highlight" (- 1 (getvar "highlight"))))
		(princ "disabled. ")
		(princ "enabled. ")
	)
	(princ)
)

(defun c:ld (/ lderr oer na1 na)
	(defun lderr (s)
		(if (/= s "Function cancelled")
			(princ (strcat "\nError:" s))
		)
		(setq ld_na na1 na1 nil)
		(if ai_beep (ai_beep))
		(setq *error* oer oer nil lderr nil)
		(princ)
	)
	(setq oer *error* *error* lderr)
	(if ld_na
		nil
		(setq ld_na "Fish")
	)
	(setq na1 ld_na na (getstring (strcat "\nAutoLISP application source to load: <" ld_na "> ")))
	(if (= na "")
		(setq na ld_na)
	)
	(if (findfile (strcat na ".lsp"))
		(progn 
			(princ "\nLoading AutoLISP application ... ")
			(setq ld_na na)
			(load na)
		)
		(alert "File not find")
	)
	(setq *error* oer)
	(princ)
)

(defun c:ldr (/ cl olderr)
	(defun *error* (s)
		(setvar "clayer" (if cl cl "0"))
		(setq *error* olderr olderr nil)
		(princ)
	)
	(setq cl (getvar "clayer"))
	(setvar "cmdecho" 0)
	(cmd "layer" "m" FLLT_DIM0 "")
	(setvar "cmdecho" 1)
	(cmd "leader" pause pause "")
	(setvar "clayer" cl)
	(setq *error* odlerr)
	(princ)
)

(defun c:len () (cmd "lengthen" "dy") (princ))

(defun c:lf (/ en lay)
	(setvar "cmdecho" 0)
	(if last_frz_lay
		(progn
			(princ (strcat "\nLast layer frozen: " (strcase last_frz_lay)))
			(initget "Thaw")
			(setq en (entsel "\n[Thaw last]<Pick an entity>: "))
		)
		(setq en (entsel "\nPick an entity: "))
	)
	(cond
		((= en "Thaw")
			(cmd "_.layer" "t" last_frz_lay "")
			(setq last_frz_lay nil)
		)
		(en
			(setq lay (cdr (assoc 8 (entget (car en)))))
			(cmd "_.layer" "f" lay "")
			(if (= lay (getvar "clayer"))
				nil
				(progn
					(setq last_frz_lay lay)
					(princ (strcat "\nLayer " lay " frozen. "))
				)
			)
		)
		(t)
	)
	(setvar "cmdecho" 1)
	(princ)
)

(defun c:lo (/ en lname stat str)
	(prompt "\nPick an entity on target layer:")
	(if (setq en (entsel)) 
		(progn
			(setq 
				lname (cdr (assoc 8 (entget (car en))))
				stat(cdr (assoc 70 (tblsearch "layer" lname)))
			)
			(if (= (logand stat 4) 4)
				(setq exec "u" str " un")
				(setq exec "lo" str " ")
			)
			(setvar "cmdecho" 0)
			(cmd "_.layer" exec lname "")
			(princ (strcat "\n" lname "-layer" str "locked, now. "))
			(setvar "cmdecho" 1)
		)
	)
	(princ)
)

(defun c:md () (fl_make_modi_layer) (princ))

(defun c:mm () (cmd "_.layer" "m") (princ))

(defun c:of (/ of_dis dis en)
	(prompt "\nOffset and erase the origenal entity.")
	(princ "\nOffset distance or Through <")
	(setq of_dis (getvar "offsetdist"))
	(if (<= of_dis 0)
		(princ "Through")
		(princ of_dis)
	)
	(initget "Through")
	(setq dis (getdist ">:"))
	(if dis nil (setq dis of_dis))
	(if (= dis "Through") (setq dis -1))
	(if (<= dis 0) (setq dis "Through"))
	(setvar "cmdecho" 0)
	(cmd "_offset" dis)
	(setvar "cmdecho" 1)
	(while (setq en (entsel))
		(cmd en pause)
		(entdel (car en))
		(setq en nil)
	)
	(cmd "")
	(princ)
)

(defun c:os ()
	(princ "\nOSMODE reset to ")
	(princ (setvar "osmode" (- 175 (* (/ (logand 175 (getvar "osmode")) 175) 175))))
	(princ)
)

(defun c:pgp () (startapp "notepad" (findfile "acad.pgp")) (princ))

(defun c:pl0 (/ olderr pw)
	(setq olderr *error*)
	(defun *error* (s)
		(if pw (setvar "plinewid" pw))
		(setq *error* olderr olderr nil)
		(princ)
	)
	(setq pw (getvar "plinewid"))
	(setvar "plinewid" 0.0)
	(cmd "pline")
	(while (= (getvar "cmdnames") "PLINE")
		(cmd pause)
	)
	(setvar "plinewid" pw)
	(setq *error* olderr)
	(princ)
)

(defun fl_xline (mtd / olderr cla)
	(defun *error* (s)
		(if cla (setvar"clayer" cla))
		(setq cla nil *error* (if olderr olderr) olderr nil)
	)
	(setq cla (getvar "clayer"))
	(setq qxset_lay (if qxset_lay qxset_lay 1))
	(if (= qxset_lay 1)
		(setvar "clayer" "DEFPOINTS")
	)
	(cmd "xline" mtd)
	(while (= (getvar "cmdnames") "XLINE")
		(cmd pause)
	)
	(setvar "clayer" cla)
	(setq *error* olderr olderr nil)
)

(defun c:qxset ()
	(setq qxset_lay (if qxset_lay qxset_lay 1))
	(setq qxset_lay (- 1 qxset_lay))
	(if (= qxset_lay 1)
		(princ "\nQX layer: DEFPOINTS ")
		(princ "\nQX layer: Current ")
	)
	(princ)
)

(defun c:qh () (fl_xline "h") (princ))

(defun c:qv () (fl_xline "v") (princ))

(defun c:qx ()
	(fl_xline (getpoint "Specify a point: "))
	(princ)
)

(defun c:r ()
	(fl_undo_begin)
	(setvar "cmdecho" 0)
	(cmd "zoom" "1x" "zoom" "p")
	(cmd "redrawall")
	(setvar "cmdecho" 1)
	(fl_undo_end)
	(princ)
)

(defun c:rds (/ os ss)
	(setq os (getvar "osmode"))
	(setvar "cmdecho" 0)
	(setvar "osmode" 0)
	(if (setq ss (ssget))
		(progn
			(princ "\nRedrawing ... ")
			(cmd "move" ss "" "0,0" "0,0")
			(princ "done. ")
		)
	)
	(setvar "cmdecho" 1)
	(setvar "osmode" os)
	(princ)
)

(defun c:rb (/ pt0 os lay_f_list lay_l_list lay itm)
	(fl_undo_begin)
	(defun append_lay_byname (lay_list lay_name)
		(append lay_list (list (cdr (assoc 2 lay_name))))
	)
	(if (setq pt0 (getpoint "\nNew point for base: "))
		(progn
			(setvar "cmdecho" 0)
			; save layer F/T L/U state
			(setq 
				lay_f_list (list)
				lay_l_list (list)
				lay (tblnext "layer" t)
			)
			(while lay
				(if (= (logand (cdr (assoc 70 lay)) 1) 1);freezed
					(setq lay_f_list (append_lay_byname (lay_f_list lay)))
				)
				(if (= (logand (cdr (assoc 70 lay)) 4) 4);locked
					(setq lay_l_list (append_lay_byname (lay_l_list lay)))
				)
				(setq lay (tblnext "layer"))
			)
			; end == save layer F/T L/U state
			(cmd "layer" "t" "*" "")
			(cmd "layer" "u" "*" "")
			(cmd "layer" "on" "*" "")
			(setq os (getvar "osmode"))
			(setvar "osmode" 0)
			(setvar "highlight" 0)
			(cmd "move" "all" "" pt0 "0,0,0")
			(cmd "base" "0,0,0")
			(cmd "zoom" "e")
			(setvar "cmdecho" 1)
			(setvar "highlight" 1)
			(setvar "osmode" os)
			;<< restore layer state
			(if lay_f_list 
				(cmd "layer" "f" (apply 'strcat (mapcar '(lambda (x) (strcat x ",")) lay_f_list)) "")
			)
			(if lay_l_list
				(cmd "layer" "lo" (apply 'strcat (mapcar '(lambda (x) (strcat x ",")) lay_l_list)) "")
			)
			;restore layer state >>
		)
	)
	(fl_undo_end)
	(princ)
)

(defun c:s (/ en flt lay)
	(setq en (entsel "\nPick an entity on target layer: "))
	(if en
		(progn
			(setq lay (cdr (assoc 8 (entget (car en)))))
			(princ (strcat "\nSelect entities on layer " (strcase lay) ": "))
			(setq flt (ssget (list (cons 8 lay))))
		)
	)
	(if (and flt (/= 0 (getvar "cmdactive")))
		flt
		(princ)
	)
)

(defun c:ssl (/ flt lay)
	(setq lay (getstring "\nLayer name: "))
	(if (= lay "") nil
		(progn
			(setq lay (strcat "*" lay "*"))
			(setq flt (ssget "x" (list (cons 8 lay))))
		);progn
	)
;(if (and flt (/= 0 (getvar "cmdactive")))
;flt
;(princ)
;)
	 (cmd "pselect" "p" "")
)


(defun c:se (/ ky flt)
	(if se_mod nil (setq se_mod "Line"))
	(initget "ARC ATtdef Circle Text Line Insert lwPolyline Dimension Hatch Solid Viewport Mtext pOint leadeR")
	(princ "\nEntity mode: [Line/ARC/ATtdef/Circle/Text/Mtext/Insert/lwPolyline/Dimension/Hatch/Solid/Viewport/pOint/leadeR]")
	(princ "\nFilter: <")
	(princ se_mod)
	(princ "> ")
	(setq ky (getkword))
	(if ky nil (setq ky se_mod))
	(setq se_mod ky)
	(setq flt (ssget (list (cons 0 se_mod))))
	(if (/= 0 (getvar "cmdactive"))
		flt
		(princ)
	)
)

(defun c:ssb (/ str ss)
	(setq str (getstring "\nInsert name to select: "))
	(if (= str "") nil
		(setq ss (ssget (list '(0 . "INSERT") (cons 2 str))))
	)
	(if (/= 0 (getvar "cmdactive"))
		ss
		(princ)
	)
)

(defun c:ssc (/ cidx ss)
	(setq cidx (getint "\nColor index to select: "))
	(if cidx
		(setq ss (ssget (list (cons 62 cidx))))
	)
	(if (/= 0 (getvar "cmdactive"))
		ss
		(princ)
	)
)

(defun c:sst (/ str ss)
	(setq str (getstring "\nText to select: "))
	(if (= str "") nil
		(setq ss (ssget (list '(0 . "TEXT") (cons 1 str))))
	)
	(if (/= 0 (getvar "cmdactive"))
		ss
		(princ)
	)
)

(defun c:stt ()
	(setq stt_stt (not stt_stt))
	(if stt_stt
		(setvar "modemacro"
			(strcat
				"标注比例：$(getvar,dimscale)  "
				"线型比例：$(getvar,ltscale)  "
				"UCS跟随：$(if,$(=,$(getvar,ucsfollow),1),否,是)  "
				"组选择：$(if,$(=,$(getvar,pickstyle),1),是,否)  "
			)
		)
		(setvar "modemacro" "")
	)
	(princ)
)

(defun c:th (/ ss)
	(setq ss (ssget))
	(if ss
		(cmd "chprop" ss "" "t" pause "")
	)
	(princ)
)

(defun c:t (/ en lname)
	(setq en (entsel "\nPick an entity on the target layer <1>:"))
	(if en
		(setq lname (cdr (assoc 8 (entget (car en)))))
		(setq lname "1")
	)
	(if (tblsearch "layer" "1")
		(setvar "clayer" lname)
		(cmd "layer" "m" "1" "c" "254" "1" "")
	)
	(princ (strcat "\nCurrent layer: " lname))
	(princ)
)

(defun c:0 () (setvar "clayer" "0") (princ))

(defun c:tm ()
	(setvar "tilemode" (- 1 (getvar "tilemode")))
	(princ)
)

(defun c:uf ()
	(princ "\nUcsfollow: ")
	(princ (setvar "ucsfollow" (- 1 (getvar "ucsfollow"))))
	(princ)
)

(defun c:uv ()
	(princ "\nUcsvp: ")
	(princ (setvar "ucsvp" 0))
	(princ)
)

(defun c:v1 () (cmd "_vports" "si") (princ))
	
(defun c:vr (/ vlist)
	(defun vlist (/ a)
		(setq a (if (tblnext "view" t) 1 0))
		(while (tblnext "view") (setq a (1+ a)))
		a
	)
	(setvar "cmdecho" 0)
	(princ "\n")
	(princ (vlist))
	(princ " View(s) found. \n")
	(cmd "_view" "r")
	(setvar "cmdecho" 1)
	(princ)
)

(defun c:vs (/ vlist vw k)
	(defun vlist (/ a)
		(setq a (if (tblnext "view" t) 1 0))
		(while (tblnext "view") (setq a (1+ a)))
		a
	)
	(setvar "cmdecho" 0)
	(princ "\n")
	(princ (vlist))
	(princ " View(s) found. \n")
		(setq vw (getstring "\nView name to save: "))
		(if (= vw "")
			 nil
			(progn
				(if (tblsearch "view" vw)
					(progn
						(initget "Yes No")
						(setq k (getkword "\nView already exist. Overwrite? Yes/<No> "))
						(if k nil (setq k "No"))
						(if (= k "Yes") (cmd "view" "s" vw))
					)
					(cmd "view" "s" vw)
				)
		 )
	)
	(setvar "cmdecho" 1)
	(princ)
)

(defun c:x () (cmd "_zoom" "p") (princ))

(defun c:xd (/ ss)
	(fl_undo_begin)
	(prompt "\nSelect Intert Object(s): ")
	(setq ss (ssget))
	(setvar "cmdecho" 0)
	(if ss (cmd "xclip" ss "" "d"))
	(setvar "cmdecho" 1)
	(fl_undo_end)
	(princ)
)

(defun c:xg (/ ss)
	(fl_undo_begin)
	(prompt "\nSelect Intert Object(s): ")
	(setq ss (ssget))
	(setvar "cmdecho" 0)
	(if ss (cmd "xclip" ss "" "p"))
	(setvar "cmdecho" 1)
	(fl_undo_end)
	(princ)
)

(defun c:xrld () (cmd "xref" "reload" "*") (princ))
(defun c:zd () (cmd "zoom" "d") (princ))
(defun c:ze () (princ))

;
; ================== Autoload Section ========================
;

(defun c:2shx () (load "hz2.lsp") (c:2shx) (princ))
(defun c:4rc () (load "4rc.lsp") (c:4rc) (princ))
(defun c:aid () (load "aid.lsp") (c:aid) (princ))
(defun c:aidd () (load "aidd.lsp") (c:aidd) (princ))
(defun c:al () (load "dim14.lsp") (c:al) (princ))
(defun c:algdim () (load "algdim.lsp") (c:algdim) (princ))
(defun c:applist () (load "xdata.lsp") (c:applist) (princ))
(defun c:arynum () (load "arynum.lsp") (c:arynum) (princ))
(defun c:at () (load "algtxt.lsp") (c:at) (princ))
(defun c:atl () (load "altx2l.lsp") (c:atl) (princ))
(defun c:atlo () (load "auto_layout.lsp") (c:atlo) (princ))
(defun c:atxt () (load "atxt.lsp") (c:atxt) (princ))
(defun c:aw () (load "awin2.lsp") (c:aw) (princ))
(defun c:bh () (load "bricks.lsp") (c:bh) (princ))
(defun c:bkc () (load "bkc.lsp") (c:bkc) (princ))
(defun c:bm () (load "brkmark.lsp") (c:bm) (princ))
(defun c:brk () (load "brk.lsp") (c:brk) (princ))
(defun c:bta () (load "ltl.lsp") (c:bta) (princ))
(defun c:bv () (load "bricks.lsp") (c:bv) (princ))
(defun c:cas () (load "fur.lsp") (c:cas) (princ))
(defun c:cf () (load "cf2.lsp") (c:cf) (princ))
(defun c:chbk () (load "chbk.lsp") (c:chbk) (princ))
(defun c:chsc () (load "chsc.lsp") (c:chsc) (princ))
(defun c:chsp () (load "chsp2.lsp") (c:chsp) (princ))
(defun c:cn () (load "dim14.lsp") (c:cn) (princ))
(defun c:cnt () (load "cnt3.lsp") (c:cnt) (princ))
(defun c:cut () (load "cut.lsp") (c:cut) (princ))
(defun c:cva () (load "cva.lsp") (c:cva) (princ))
(defun c:dd () (load "ddf_edit.lsp") (c:dd) (princ))
(defun c:ddr () (load "ddr.lsp") (c:ddr) (princ))
(defun c:ddst () (load "ddf_st.lsp") (c:ddst) (princ))
(defun c:df () (load "df.lsp") (c:df) (princ))
(defun c:dft () (load "dd2of.lsp") (c:dft) (princ))
(defun c:dg () (load "sc4.lsp") (c:dg) (princ))
(defun c:dimbrk () (load "dimbrk2.lsp") (c:dimbrk) (princ))
(defun c:dimdr () (load "dimdr.lsp") (c:dimdr) (princ))
(defun c:dl () (load "dimlin2.lsp") (c:dimlin2) (princ))
;(defun c:dl () (load "dimlinc.lsp") (c:dimlin) (princ))
(defun c:dimuni () (load "dimuni.lsp") (c:dimuni) (princ))
(defun c:div () (load "divide.lsp") (c:div) (princ))
(defun c:dr () (load "door.lsp") (c:door) (princ))
(defun c:drwv () (load "drwv.lsp") (c:drwv) (princ))
(defun c:dsc () (load "sc4.lsp") (c:dsc) (princ))
(defun c:dse () (load "dimse.lsp") (c:dse) (princ))
(defun c:dw () (load "op_wall.lsp") (c:dw) (princ))
(defun c:dwgn () (load "dwgn.lsp") (c:dwgn) (princ))
(defun c:edr () (load "edr.lsp") (c:edr) (princ))
(defun c:eladd () (load "reel.lsp") (c:eladd) (princ))
(defun c:elin () (load "elins3.lsp") (c:elin) (princ))
(defun c:et () (load "et4.lsp") (c:et) (princ))
(defun c:exof () (load "exof.lsp") (c:exof) (princ))
(defun c:fft () (load "findt.lsp") (c:fft) (princ))
(defun c:fixt () (load "addp.lsp") (c:fixt) (princ))
(defun c:flat () (load "flat.lsp") (c:flat) (princ))
(defun c:fnd () (load "fnd.lsp") (c:fnd) (princ))
(defun c:fst () (load "f_stair.lsp") (c:fst) (princ))
(defun c:gd () (load "siteutil.lsp") (c:gd) (princ))
(defun c:gm () (load "siteutil.lsp") (c:gm) (princ))
(defun c:gr () (load "siteutil.lsp") (c:gr) (princ))
(defun c:gsc () (load "gsc.lsp") (c:gsc) (princ))
(defun c:h31 () (load "hc.lsp") (c:h31) (princ))
(defun c:hc () (load "hc.lsp") (c:hc) (princ))
(defun c:hcc () (load "hcx.lsp") (c:hcc) (princ))
(defun c:hcx () (load "hcx.lsp") (c:hcx) (princ))
(defun c:hd () (load "hide.lsp") (c:hd) (princ))
(defun c:hs () (load "hc.lsp") (c:hs) (princ))
(defun c:insaxis () (load "zxy.lsp") (c:insaxis) (princ))
(defun c:insnum () (load "zxy.lsp") (c:insnum) (princ))
(defun c:j () (load "jline.lsp") (c:j) (princ))
(defun c:l2w () (load "wall-1x.lsp") (c:l2w) (princ))
(defun c:layerin () (load "layerout.lsp") (c:layerin) (princ))
(defun c:layerout () (load "layerout.lsp") (c:layerout) (princ))
(defun c:lca () (load "lca.lsp") (c:lca) (princ))
(defun c:ldir () (load "ldir.lsp") (c:ldir) (princ))
(defun c:ldr () (load "dim14.lsp") (c:ldr) (princ))
(defun c:llx () (load "llx.lsp") (c:llx) (princ))
(defun c:lm () (load "lm.lsp") (c:lm) (princ))
(defun c:ltl () (load "ltl.lsp") (c:ltl) (princ))
(defun c:lw () (load "plw.lsp") (c:lw) (princ))
(defun c:mdw () (load "wall-fun.lsp") (c:mdw) (princ))
(defun c:mf () (load "mf.lsp") (c:mf) (princ))
;(defun c:mkclm () (load "mkclm2.lsp") (c:mkclm) (princ))
(defun c:mksld () (load "mksld.lsp") (c:mksld) (princ))
(defun c:mkvp () (load "mv.lsp") (c:mkvp) (princ))
(defun c:mo () (load "mo.lsp") (c:mo) (princ))
(defun c:mrgt () (load "addp.lsp") (c:mrgt) (princ))
(defun c:ow () (load "ow.lsp") (c:ow) (princ))
(defun c:pkrid () (load "parkutil.lsp") (c:pkrid) (princ))
(defun c:plwsc () (load "plw2.lsp") (c:plwsc) (princ))
(defun c:pst () (load "pst.lsp") (c:pst) (princ))
(defun c:qhh () (load "qhatch.lsp") (c:qhh) (princ))
(defun c:qhs () (load "qhatch.lsp") (c:qhs) (princ))
(defun c:rct () (load "rect.lsp") (c:rct) (princ))
(defun c:rect () (load "rect.lsp") (c:rect) (princ))
(defun c:reel () (load "reel.lsp") (c:reel) (princ))
(defun c:reini () (load "reini2.lsp") (princ))
(defun c:renum () (load "zxy.lsp") (c:renum) (princ))
(defun c:rnb () (load "rnb.lsp") (c:rnb) (princ))
(defun c:rsdr () (load "rsdr.lsp") (c:rsdr) (princ))
(defun c:rta () (load "rta.lsp") (c:rta) (princ))
(defun c:saa () (load "saa.lsp") (c:saa) (princ))
(defun c:scaxi () (load "scaxi.lsp") (c:scaxi) (princ))
(defun c:scel () (load "scel.lsp") (c:scel) (princ))
(defun c:scset () (load "sc4_a.lsp") (c:scset) (princ))
(defun c:sctx () (load "txthi.lsp") (c:sctx) (princ))
(defun c:sf () (load "sf.lsp") (c:sf) (princ))
(defun c:srp () (load "srp.lsp") (c:srp) (princ))
(defun c:str+ () (load "addp.lsp") (c:str+) (princ))
;(defun c:swb () (load "wall-up.lsp") (c:swb) (princ))
(defun c:te () (load "te.lsp") (c:te) (princ))
(defun c:txtin () (load "cvtext.lsp") (c:txtin) (princ))
(defun c:txtout () (load "cvtext.lsp") (c:txtout) (princ))
(defun c:ue () (load "ue.lsp") (c:ue) (princ))
(defun c:uef () (load "ue.lsp") (c:uef) (princ))
(defun c:vh () (load "dim14.lsp") (c:vh) (princ))
(defun c:vpl () (load "vpl.lsp") (c:vpl) (princ))
(defun c:vv () (load "vv.lsp") (c:vv) (princ))
(defun c:wa () (load "wa.lsp") (c:wa) (princ))
(defun c:wall () (load "wall-2x.lsp") (c:wall) (princ))
(defun c:wchk () (load "wall-up.lsp") (c:wchk) (princ))
(defun c:win () (load "win3.lsp") (c:win) (princ))
(defun c:wrid () (load "parkutil.lsp") (c:wrid) (princ))
(defun c:wup () (load "wall-up.lsp") (c:wup) (princ))
(defun c:xc () (load "#_cross.lsp") (c:xc) (princ))
(defun c:xdata () (load "xdata.lsp") (c:xdata) (princ))
(defun c:xlt () (load "xref_layers_traverse.lsp") (c:xlt) (princ))
(defun c:xrr () (load "xrefutil.lsp") (c:xrr) (princ))
(defun c:xru () (load "xrefutil.lsp") (c:xru) (princ))
(defun c:xrd () (load "xrefutil.lsp") (c:xrd) (princ))
(defun c:xrbb () (load "xrefutil.lsp") (c:xrbb) (princ))
(defun c:xrbi () (load "xrefutil.lsp") (c:xrbi) (princ))
(defun c:xin () (load "xin.lsp") (c:xin) (princ))
(defun c:zb () (load "zb4.lsp") (c:zb) (princ))
;
; ================== TEMP ========================
;
;
; ================== Loaded infomation ===========
;

(setvar "gridmode" 0)

(if ld_na
	(princ "loaded.\nFishLISP. ")
	(princ "\nFishLISP loaded. ")
)
(princ)