(load "./rt.cl")

(declaim (optimize (speed 3)
				   (compilation-speed 0)
				   (safety 0)
				   (debug 0)))

(defun rd-rt-libname (str-in oldlist)
  (let ((bt (rd-u2 str-in)))
	(if (= bt *RT_LIBNAME*)
	  (progn
		(setq oldlist (push (reverse (list (rd-dt-string str-in "") "RT_LIBNAME")) oldlist))
		(sgk-f003 str-in oldlist))
	  (progn
		(rd-rt-libname str-in oldlist)))))

(defun rd-dt-bitarray (str-in len num)
  (let ((lst '()))
	(dotimes (n num)
	  (cond
		((= len 2) (push (rd-u2 str-in) lst))
		((= len 4) (push (rd-u4 str-in) lst))))
	(make-array (length lst) :initial-contents (reverse lst))
	))

(cl:defpackage "TEST-C-CALL" (:use "CL" "SB-ALIEN" "SB-C-CALL"))
(load-shared-object "./rw.so")
(define-alien-routine r642d double-float
  (v unsigned-long))
(define-alien-routine d2r64 unsigned-long
  (v double-float))

(defun is-name? (asii)
  (let ((c (code-char asii)))
  (if 	(or
		(and (char>= c #\A) (char<= c #\Z))
		(and (char>= c #\a) (char<= c #\z))
		(and (char>= c #\0) (char<= c #\9))
		(char= c #\_)
		(char= c #\!)
		(char= c #\$)
;		(char= c #\()
;		(char= c #\))
		(char= c #\[)
		(char= c #\])
		(char= c #\<)
		(char= c #\>)
		)
	t
	nil)))

(defun push-char-to-string (bt stri)
  (concatenate 'string stri (string (code-char bt))))

(defun rd-dt-string (str-in stri)
  (let ((bt (read-byte str-in)))
	(if (is-name? bt)
	  (progn
		(setq stri (push-char-to-string bt stri))
		(rd-dt-string str-in stri))
	  (string stri))))

(defun rd-dt-string4 (str-in)
  (let ((bt) (stri ""))
	(loop
	  (setf bt (rd-u2 str-in))
	  (if (is-name? (ldb (byte 8 8) bt))
		 (setq stri (push-char-to-string (ldb (byte 8 8) bt) stri))
		 nil)
	  (if (is-name? (ldb (byte 8 0) bt))
		 (setq stri (push-char-to-string (ldb (byte 8 0) bt) stri))
		 nil)
	  (when (= bt #x0004)
		(return (string stri))))))

(defun rd-dt-string6 (str-in)
  (let ((bt) (stri ""))
	(loop
	  (setf bt (rd-u2 str-in))
	  (if (is-name? (ldb (byte 8 8) bt))
		 (setq stri (push-char-to-string (ldb (byte 8 8) bt) stri))
		 nil)
	  (if (is-name? (ldb (byte 8 0) bt))
		 (setq stri (push-char-to-string (ldb (byte 8 0) bt) stri))
		 nil)
	  (when (= bt #x0006)
		(return (string stri))))))

(defun rd-dt-string3 (str-in stri)
  (let ((bt (rd-u2 str-in)))
	(if (or
		  (is-name? (ldb (byte 8 0) bt))
		  (is-name? (ldb (byte 8 8) bt))
		  )
	  (progn
		(if (is-name? (ldb (byte 8 8) bt))
		 (setq stri (push-char-to-string (ldb (byte 8 8) bt) stri))
		 nil)
		(if (is-name? (ldb (byte 8 0) bt))
		  (setq stri (push-char-to-string (ldb (byte 8 0) bt) stri))
		  nil)
		(rd-dt-string3 str-in stri))
	  (if (or
			(= bt #x0000)
			(= bt #x0004)
			(= bt #x0006)
			)
		(string stri)
		(rd-dt-string3 str-in stri)))))

(defun rd-u2 (str-in)
  (let ((u2 0) (eof 0))
	(setf (ldb (byte 8 8) u2) (read-byte str-in nil eof))
	(setf (ldb (byte 8 0) u2) (read-byte str-in nil eof))
	u2)
  )

(defun rd-u4 (str-in)
  (let ((u4 0))
	(setf (ldb (byte 16 16) u4) (rd-u2 str-in))
	(setf (ldb (byte 16 0) u4) (rd-u2 str-in))
	u4)
  )

(defun rd-u8 (str-in)
  (let ((u8 0))
	(setf (ldb (byte 32 32) u8) (rd-u4 str-in))
	(setf (ldb (byte 32 0) u8) (rd-u4 str-in))
	u8)
  )

(defun rd-u16 (str-in)
  (let ((u16 0))
	(setf (ldb (byte 64 64) u16) (rd-u8 str-in))
	(setf (ldb (byte 64 0) u16) (rd-u8 str-in))
	u16)
  )

(defun wt-u2 (str-out var)
  (write-byte (ldb (byte 8 8) var) str-out)
  (write-byte (ldb (byte 8 0) var) str-out)
  )

(defun wt-u4 (str-out var)
  (wt-u2 str-out (ldb (byte 16 16) var))
  (wt-u2 str-out (ldb (byte 16 0) var))
  )

(defun wt-u8 (str-out var)
  (wt-u4 str-out (ldb (byte 32 32) var))
  (wt-u4 str-out (ldb (byte 32 0) var))
  )

(defun wt-u16 (str-out var)
  (wt-u8 str-out (ldb (byte 64 64) var))
  (wt-u8 str-out (ldb (byte 64 0) var))
  )

(defun f-return-srctree (fi)
  (let ((srctree '()))
	(with-open-file (str-in fi :direction	:input
							:element-type	'(unsigned-byte 8))
	  (setq srctree (sgk-f003 str-in srctree))
	  (reverse (push "RT_ENDLIB" srctree)))))

(defun sgk-f003 (str-in oldlist)
  (let ((bt (rd-u2 str-in)) (newlist '()))
	(cond
	  ((= bt *RT_BGNLIB*) (progn
							(push (list
									"RT_BGNLIB"
									(rd-dt-bitarray str-in 2 12)
									)
								  oldlist)
							(rd-rt-libname str-in oldlist)
							))
	  ((= bt *RT_HEADER*) (progn
							(push (list "RT_HEADER" (rd-u2 str-in)) oldlist)
							(sgk-f003 str-in oldlist)
							))
	  ((= bt *RT_UNITS*) (progn
						   (push "RT_UNITS" newlist)
						   (push (r642d (rd-u8 str-in)) newlist)
						   (push (r642d (rd-u8 str-in)) newlist)
						   (push (reverse (car (sgk-f004 str-in newlist))) oldlist)
						   ))
	  (t (sgk-f003 str-in oldlist)))))

(defun sgk-f001 (str-in oldlist)
  (let ((bt (read-byte str-in nil 'eof)))
	(if (eql bt 'eof)
	  (list oldlist)
	  (sgk-f001 str-in oldlist))))

(defun sgk-f004 (str-in oldlist)
  (let ((bt (rd-u2 str-in)) (newlist '()))
	(if (eql bt *RT_ENDLIB*)
	  (progn
		(sgk-f001 str-in oldlist))
	  (cond
		((= bt *RT_BGNSTR*) (progn
							  (push (reverse (sgk-f005 str-in)) oldlist)
							  (sgk-f004 str-in oldlist)
							  ))
		(t (sgk-f004 str-in oldlist))))))

(defvar strlist '())
(defvar elm '())

(defun sgk-f005 (str-in)
  (let ((bt) (rtl))
  (setq strlist (reverse (list 
				  (list "RT_BGNSTR" (rd-dt-bitarray str-in 2 12)
						)
				  (list "RT_STRNAME" (rd-dt-string4 str-in)
						)
					  )))
  (loop
	(setq bt (rd-u2 str-in))
	(setq elm '())
	(cond
	  ((= bt *RT_BOUNDARY*) (progn
							  (sgk-f006 str-in)
							  (push (reverse elm) strlist)
							  ))
	  ((= bt *RT_PATH*) (progn
						  (sgk-f016 str-in)
						  (push (reverse elm) strlist)
						  ))
	  ((= bt *RT_SREF*) (progn
						  (sgk-f026 str-in)
						  (push (reverse elm) strlist)
						  ))
	  ((= bt *RT_AREF*) (progn
						  (sgk-f036 str-in)
						  (push (reverse elm) strlist)
						  ))
	  ((= bt *RT_TEXT*) (progn
						  (sgk-f046 str-in)
						  (push (reverse elm) strlist)
						  ))
	  ((= bt *RT_NODE*) (progn
						  (sgk-f056 str-in)
						  (push (reverse elm) strlist)
						  ))
	  ((= bt *RT_BOX*) (progn
						 (sgk-f066 str-in)
						 (push (reverse elm) strlist)
						 ))
	  )
	(when (= bt *RT_ENDSTR*)
	  (return (push "RT_ENDSTR" strlist))))))

(defun sgk-f006 (str-in)
  (let ((bt 0) (xy) (rtl))
	(setq elm '("RT_BOUNDARY"))
	(loop
	  (setf rtl (- (logior bt) 4))
	  (setq bt (rd-u2 str-in))
	  (cond
		((= bt *RT_LAYER*) (progn
							 (push (list "RT_LAYER" (rd-u2 str-in)) elm)
							 ))
		((= bt *RT_DATATYPE*) (progn
								(push (list "RT_DATATYPE" (rd-u2 str-in)) elm)
								))
		((= bt *RT_XY*) (push (list "RT_XY"
									(rd-dt-bitarray str-in 4 (/ rtl 4))
									) elm))
		)
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(return elm)))))

(defun sgk-f016 (str-in)
  (let ((bt 0) (xy) (rtl))
	(setq elm '("RT_PATH"))
	(loop
	  (setf rtl (- (logior bt) 4))
	  (setq bt (rd-u2 str-in))
	  (cond
		((= bt *RT_LAYER*) (progn
							 (push (list "RT_LAYER" (rd-u2 str-in)) elm)
							 ))
		((= bt *RT_DATATYPE*) (progn
								(push (list "RT_DATATYPE" (rd-u2 str-in)) elm)
								))
		((= bt *RT_PATHTYPE*) (progn
								(push (list "RT_PATHTYPE" (rd-u2 str-in)) elm)
								))
		((= bt *RT_WIDTH*) (progn
							 (push (list "RT_WIDTH" (logior (rd-u4 str-in))) elm)
							 ))
		((= bt *RT_XY*) (push (list "RT_XY"
									(rd-dt-bitarray str-in 4 (/ rtl 4))
									) elm))
		)
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(return elm)))))

(defun sgk-f026 (str-in)
  (let ((bt 0) (xy) (rtl))
	(setq elm '( "RT_SREF"))
	(loop
	  (setf rtl (- (logior bt) 4))
	  (setq bt (rd-u2 str-in))
	  (cond
		((= bt *RT_SNAME*) (progn
							 (push (list "RT_SNAME" (rd-dt-string6 str-in)) elm)
							 ))
		((= bt *RT_STRANS*) (progn
							  (push (list "RT_STRANS" (rd-u2 str-in)) elm)
							  ))
		((= bt *RT_PATHTYPE*) (progn
								(push (list "RT_PATHTYPE" (rd-u2 str-in)) elm)
								))
		((= bt *RT_ANGLE*) (progn
							 (push (list "RT_ANGLE" (r642d (rd-u8 str-in))) elm)
							 ))
		((= bt *RT_XY*) (push (list "RT_XY"
									(rd-dt-bitarray str-in 4 (/ rtl 4))
									) elm))
		)
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(return elm)))))

(defun sgk-f036 (str-in)
  (let ((bt 0) (rtl))
	(setq elm '("RT_AREF"))
	(loop
	  (setf rtl (- (logior bt) 4))
	  (setq bt (rd-u2 str-in))
	  (cond
		((= bt *RT_SNAME*) (progn
							 (push (list "RT_SNAME" (rd-dt-string6 str-in)) elm)
							 ))
		((= bt *RT_STRANS*) (progn
							  (push (list "RT_STRANS" (rd-u2 str-in)) elm)
							  ))
		((= bt *RT_ANGLE*) (progn
							 (push (list "RT_ANGLE" (r642d (rd-u8 str-in))) elm)
							 ))
		((= bt *RT_COLROW*) (progn
							  (push (list "RT_COLROW" (rd-dt-bitarray str-in 2 2)) elm)
							  ))
		((= bt *RT_XY*) (push (list "RT_XY"
									(rd-dt-bitarray str-in 4 (/ rtl 4))
									) elm))
		)
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(return elm)))))

(defun sgk-f046 (str-in)
  (let ((bt 0) (rtl 0))
	(setq elm '("RT_TEXT"))
	(loop
	  (setf rtl (- (logior bt) 4))
	  (setq bt (rd-u2 str-in))
	  (cond
		((= bt *RT_LAYER*) (progn
							 (push (list "RT_LAYER" (rd-u2 str-in)) elm)
							 ))
		((= bt *RT_TEXTTYPE*) (progn
								(push (list "RT_TEXTTYPE" (rd-u2 str-in)) elm)
								))
		((= bt *RT_PRESENTATION*) (progn
									(push (list "RT_PRESENTATION" (rd-u2 str-in)) elm)
									))
		((= bt *RT_STRANS*) (progn
							  (push (list "RT_STRANS" (rd-u2 str-in)) elm)
							  ))
		((= bt *RT_MAG*) (progn
							 (push (list "RT_MAG" (r642d (rd-u8 str-in))) elm)
							 ))
		((= bt *RT_ANGLE*) (progn
							 (push (list "RT_ANGLE" (r642d (rd-u8 str-in))) elm)
							 ))
		((= bt *RT_XY*) (push (list "RT_XY"
									(rd-dt-bitarray str-in 4 (/ rtl 4))
									) elm))
		((= bt *RT_STRING*) (progn
							  (push (list "RT_STRING" (rd-dt-string4 str-in)) elm)
							  ))
	  )
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(return elm)))))

(defun sgk-f056 (str-in)
  (let ((bt) (rtl))
	(setq elm '("RT_NODE"))
	(loop
	  (setq bt (rd-u2 str-in))
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(push (rd-u2 str-in) elm)
		(return elm)))))

(defun sgk-f066 (str-in)
  (let ((bt) (rtl))
	(setq elm '("RT_BOX"))
	(loop
	  (setq bt (rd-u2 str-in))
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(push (rd-u2 str-in) elm)
		(return elm)))))

(defun flatten-tree (tree)
  (cond
	((null tree) nil)
	((atom tree) (list tree))
	(t (mapcan #'flatten-tree tree))))

(defun wt-dt-string5 (str-out stri)
  (let ((u2 0) (len (length stri)))
	(dotimes (n (floor (float (/ len 2))))
	  (setf (ldb (byte 8 8) u2) (char-code (aref stri (* 2 n))))
	  (setf (ldb (byte 8 0) u2) (char-code (aref stri (1+ (* 2 n)))))
	  (wt-u2 str-out u2)
	  )
	(if (oddp len)
	  (progn
		(setf (ldb (byte 8 8) u2) (char-code (aref stri (1- len))))
		(setf (ldb (byte 8 0) u2) #x00)
		(wt-u2 str-out u2))
	  nil)
	))

(defun wt-dt-string6 (str-out stri)
  (wt-dt-string7 str-out stri)
  (if (oddp (length stri))
	(write-byte #x2e str-out)
	nil))

(defun wt-dt-string7 (str-out stri)
  (loop for n from 0 to (1- (length stri)) do
		(write-byte (char-code (aref stri n)) str-out)))

(defun wt-dt-bitarray (str-out ary sel)
  (loop for n from 0 to (1- (array-total-size ary)) do
		(cond
		  ((= 2 sel) (wt-u2 str-out (aref ary n)))
		  ((= 4 sel) (wt-u4 str-out (aref ary n)))
		  ((= 8 sel) (wt-u8 str-out (aref ary n)))
		  )))

(defun wt-dt-real64 (str-out d)
  (wt-u8 str-out (d2r64 d)))

(defun sgk-f100 (fo tree)
  (let ((lst (flatten-tree tree)) (sel 0))
	(setq lst (make-array (length lst) :initial-contents lst))
	(with-open-file (str-out fo :direction	:output
							 :if-does-not-exist :create
							 :if-exists		:supersede
							 :element-type	'(unsigned-byte 8))
	  (loop for n from 0 to (1- (array-total-size lst)) do
			(cond
			  ((stringp (aref lst n)) (sgk-f101 str-out lst n))
			  ))
)))

(defun length1 (stri)
  (if (oddp (length stri))
	(1+ (length stri))
	(length stri)))

(defun length2 (ary))

(defun sgk-f101 (str-out lst n)
  (cond
	((string= "RT_HEADER" (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out *RT_HEADER*)
										  (wt-u2 str-out (aref lst (1+ n)))
										  (wt-u2 str-out #x001c)
										  ))
	((string= "RT_BGNLIB" (aref lst n)) (progn
										  (wt-u2 str-out *RT_BGNLIB*)
										  (wt-dt-bitarray str-out (aref lst (1+ n)) 2)
										  ))
	((string= "RT_LIBNAME" (aref lst n)) (progn
										  (wt-u2 str-out (+ 6 (* 1 (length1 (aref lst (1+ n))))))
										  (wt-u2 str-out *RT_LIBNAME*)
										  (wt-dt-string6 str-out (aref lst (1+ n)))
										  (wt-u2 str-out #x4442)
										  (wt-u2 str-out #x0014)
										  ))
	((string= "RT_UNITS" (aref lst n)) (progn
										  (wt-u2 str-out *RT_UNITS*)
										  (wt-dt-real64 str-out (aref lst (1+ n)))
										  (wt-dt-real64 str-out (aref lst (1+ (1+ n))))
										  ))
	((string= "RT_ENDLIB" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_ENDLIB*)
										  ))
	((string= "RT_BGNSTR" (aref lst n)) (progn
										  (wt-u2 str-out #x001c)
										  (wt-u2 str-out *RT_BGNSTR*)
										  (wt-dt-bitarray str-out (aref lst (1+ n)) 2)
										  ))
	((string= "RT_STRNAME" (aref lst n)) (progn
										  (wt-u2 str-out (+ 4 (* 1 (length1 (aref lst (1+ n))))))
										  (wt-u2 str-out *RT_STRNAME*)
										  (wt-dt-string5 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_BOUNDARY" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_BOUNDARY*)
										  ))
	((string= "RT_PATH" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_PATH*)
										  ))
	((string= "RT_SREF" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_SREF*)
										  ))
	((string= "RT_AREF" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_AREF*)
										  ))
	((string= "RT_TEXT" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_TEXT*)
										  ))
	((string= "RT_NODE" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_NODE*)
										  ))
	((string= "RT_BOX" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_BOX*)
										  ))
	((string= "RT_ENDSTR" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_ENDSTR*)
										  ))
	((string= "RT_LAYER" (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out *RT_LAYER*)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_DATATYPE" (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out *RT_DATATYPE*)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_PATHTYPE" (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out *RT_PATHTYPE*)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_XY" (aref lst n)) (progn
										  (wt-u2 str-out (+ 4 (* 4 (length (aref lst (1+ n))))))
										  (wt-u2 str-out *RT_XY*)
										  (wt-dt-bitarray str-out (aref lst (1+ n)) 4)
										  ))
	((string= "RT_ENDEL" (aref lst n)) (progn
										  (wt-u2 str-out #x0004)
										  (wt-u2 str-out *RT_ENDEL*)
										  ))
	((string= "RT_SNAME" (aref lst n)) (progn
										  (wt-u2 str-out (+ 4 (* 1 (length1 (aref lst (1+ n))))))
										  (wt-u2 str-out *RT_SNAME*)
										  (wt-dt-string5 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_STRANS" (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out *RT_STRANS*)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_ANGLE" (aref lst n)) (progn
										  (wt-u2 str-out #x000c)
										  (wt-u2 str-out *RT_ANGLE*)
										  (wt-dt-real64 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_COLROW" (aref lst n)) (progn
										  (wt-u2 str-out #x0008)
										  (wt-u2 str-out *RT_COLROW*)
										  (wt-dt-bitarray str-out (aref lst (1+ n)) 2)
										  ))
	((string= "RT_TEXTTYPE" (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out *RT_TEXTTYPE*)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_PRESENTATION" (aref lst n)) (progn
										  (wt-u2 str-out #x0006)
										  (wt-u2 str-out *RT_PRESENTATION*)
										  (wt-u2 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_MAG" (aref lst n)) (progn
										  (wt-u2 str-out #x000c)
										  (wt-u2 str-out *RT_MAG*)
										  (wt-dt-real64 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_STRING" (aref lst n)) (progn
										  (wt-u2 str-out (+ 4 (* 1 (length1 (aref lst (1+ n))))))
										  (wt-u2 str-out *RT_STRING*)
										  (wt-dt-string5 str-out (aref lst (1+ n)))
										  ))
	((string= "RT_WIDTH" (aref lst n)) (progn
										  (wt-u2 str-out #x0008)
										  (wt-u2 str-out *RT_WIDTH*)
										  (wt-u4 str-out (aref lst (1+ n)))
										  ))
	)
  )

(defun get-units (lib) (cdddr (cadddr lib)))

(defun is-str? (lst)
  (if (string= (caar lst) "RT_BGNSTR") t nil))

(defun get-str-name (str) (cadr (cadr str)))

(defun get-str (units name)
  (dolist (str units)
	(when (string= name (get-str-name str))
	  (return str))))
