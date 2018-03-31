(load "./3.fasl")

(declaim (optimize (speed 3)
				   (compilation-speed 0)
				   (safety 0)
				   (debug 0)))

(defun sgk-rd-gds (fi) (f-return-srctree fi))

(defun sgk-wt-gds (fo tree) (f100 fo (flatten-tree tree)))

(defun sgk-lib (name units)
  (list (list "RT_HEADER" 5)
		(list "RT_BGNLIB" #(117 1 11 16 17 41 117 1 11 16 21 29))
		(list "RT_LIBNAME" name)
		(list "RT_UNITS" 9.999999999999998d-4 9.999999999999999d-10
			  units)
		"RT_ENDLIB"))

(defun sgk-cell (name elms)
  (list (list "RT_BGNSTR" #(70 1 1 8 0 0 118 2 2 14 29 35))
		(list "RT_STRNAME" name)
		elms
		"RT_ENDSTR"))

(defun sgk-sref (sname strans angle xy)
  (list "RT_SREF" (list "RT_SNAME" sname)
	(list "RT_STRANS" strans)
	(list "RT_ANGLE" angle)
	(list "RT_XY" xy)
	"RT_ENDEL"))

(defun sgk-ls-cell (lib)
  (dolist (n (get-units lib))
  (format t "~a~%" (get-str-name n))))

