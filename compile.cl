(setf sb-ext:*evaluator-mode* :compile)
(load "./3.cl")
;(let ((process (sb-ext:run-program "/usr/bin/sbcl" '("--dynamic-space-size" "4096" "--script ./5.lisp") :wait nil)))
;(sb-ext:dynamic-space-size 4096

;(sb-ext:save-lisp-and-die "sgk"
;  :executable t
;  :toplevel 'sgk)

(declaim (optimize (speed 3)
				   (compilation-speed 0)
				   (safety 0)
				   (debug 0)))

(compile-file "./3.cl")
;(compile-file "./4.cl")
(load "./2.cl")
(compile-file "./2.cl")

