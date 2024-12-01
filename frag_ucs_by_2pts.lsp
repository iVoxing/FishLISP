(defun ucs_by_2pts ()
	(get_pts)
	(setq na (getstring "\nUCS name: "))
	(setq ori (ucs_ori) ang (ucs_ang))
	(command "_.ucs" "w" "" "o" ori "" "z" ang "" "s" na)
)

(defun get_pts ()
)

(defun scale_ratio (/ d_in_wcs d_in_dim)
	(setq d_in_wcs (sqrt (+ (expt (- pt1_x_wcs pt2_x_wcs) 2) (expt (- pt1_y_wcs pt2_y_wcs)))))
	(setq d_in_dim (sqrt (+ (expt (- pt1_x_dim pt2_x_dim) 2) (expt (- pt1_y_dim pt2_y_dim)))))
	(/ d_in_dim d_in_wcs)
)

(defun acos (/ cosA)
	(atan (sqrt (- 1 (* cosA cosA))) cosA)
)

(defun ucs_ang ()
	(acos (/ (- pt1_x_wcs pt2_x_wcs)(- pt1_x_dim pt2_x_dim)))
)

(defun ucs_ori (pt_wcs)
	(polar (polar  pt_wcs (* scale_ratiopt1_x_dim) (+ pi ucs_ang)) (* scale_ratio pt1_y_dim) (+ (* 1.5 pi) ucs_ang))
)