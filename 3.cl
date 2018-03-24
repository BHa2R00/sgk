(load "./rt.cl")
(load "./conf.cl")

(defun rd-rt-libname (str-in oldlist)
  (let ((bt (rd-u2 str-in)))
	(if (= bt *RT_LIBNAME*)
	  (progn
		(setq oldlist (push (reverse (list (rd-dt-string str-in "") "RT_LIBNAME")) oldlist))
		(f003 str-in oldlist))
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

(defun rd-dt-bitarray1 (str-in oldlist)
  (let ((bt (rd-u2 str-in)) (u4 0) (xy))
	(if (or
;		  (= bt *RT_ENDEL*)
		  (= bt #x0004)
		  (= bt #x000a)
;		  (= (ldb (byte 8 8) bt) #x00)
;		  (= (ldb (byte 8 0) bt) #x04)
;		  (>= (length oldlist) 200)
		  )
	  (progn
		(setf xy (list (make-array (length oldlist)
				  :initial-contents (reverse  oldlist))))
		(list xy (list "RT_WTF" bt)))
	  (progn
		(push
		  (progn
			(setf (ldb (byte 16 16) u4) bt)
;			(setf (ldb (byte 16 16) u4) (rd-u2 str-in))
			(setf (ldb (byte 16 0) u4) (rd-u2 str-in))
			(setf u4 (logior u4))
			)
		  oldlist)
;	(write oldlist)
		(rd-dt-bitarray1 str-in oldlist)))))

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
;		(char= c #\<)
;		(char= c #\>)
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
	  (setq srctree (f003 str-in srctree))
	  (reverse (push "RT_ENDLIB" srctree)))))

(defun f003 (str-in oldlist)
  (let ((bt (rd-u2 str-in)) (newlist '()))
	(cond
	  ((= bt *RT_BGNLIB*) (progn
							(push (list
									"RT_BGNLIB"
									(rd-dt-bitarray str-in 2 12)
									;(list "RT_WTF" (rd-u2 str-in)))
									(rd-u2 str-in))
								  oldlist)
							(rd-rt-libname str-in oldlist)
							))
	  ((= bt *RT_HEADER*) (progn
							(push (list "RT_HEADER" (rd-u2 str-in)) oldlist)
							(f003 str-in oldlist)
							))
	  ((= bt *RT_UNITS*) (progn
						   (push "RT_UNITS" newlist)
						   (push (r642d (rd-u8 str-in)) newlist)
						   (push (r642d (rd-u8 str-in)) newlist)
						   (push (reverse (car (f004 str-in newlist))) oldlist)
						   ))
	  (t (f003 str-in oldlist)))))

(defun f001 (str-in oldlist)
  (let ((bt (read-byte str-in nil 'eof)))
	(if (eql bt 'eof)
	  (list oldlist)
	  (f001 str-in oldlist))))

(defun f004 (str-in oldlist)
  (let ((bt (rd-u2 str-in)) (newlist '()))
	(if (eql bt *RT_ENDLIB*)
	  (progn
		(f001 str-in oldlist))
	  (cond
		((= bt *RT_BGNSTR*) (progn
							  (push (reverse (f005 str-in)) oldlist)
							  (f004 str-in oldlist)
							  ))
		(t (f004 str-in oldlist))))))

(defvar strlist '())
(defvar elm '())

(defun f005 (str-in)
  (let ((bt))
  (setq strlist (reverse (list 
				  (list "RT_BGNSTR" (rd-dt-bitarray str-in 2 12)
						(list "RT_WTF" (rd-u2 str-in)))
				  ;(list "RT_STRNAME" (rd-dt-string3 str-in "")
				  (list "RT_STRNAME" (rd-dt-string4 str-in)
						;(list "RT_WTF" (rd-u2 str-in)))
						(list "RT_WTF" #x0004))
					  )))
  (loop
	(setq bt (rd-u2 str-in))
	(setq elm '())
	(cond
	  ((= bt *RT_BOUNDARY*) (progn
							  ;(push (list "RT_WTF" (rd-u2 str-in)) strlist)
							  ;(push (list "RT_WTF" #x0004) strlist)
							  (f006 str-in)
							  (push (reverse elm) strlist)
							  ))
	  ((= bt *RT_PATH*) (progn
						  (f016 str-in)
						  (push (reverse elm) strlist)
						  ))
	  ((= bt *RT_SREF*) (progn
						  (f026 str-in)
						  (push (reverse elm) strlist)
						  ))
	  ((= bt *RT_AREF*) (progn
						  (f036 str-in)
						  (push (reverse elm) strlist)
						  ))
	  ((= bt *RT_TEXT*) (progn
						  (f046 str-in)
						  (push (reverse elm) strlist)
						  ))
	  ((= bt *RT_NODE*) (progn
						  (f056 str-in)
						  (push (reverse elm) strlist)
						  ))
	  ((= bt *RT_BOX*) (progn
						 (f066 str-in)
						 (push (reverse elm) strlist)
						 ))
	  ;(t (push (list "RT_WTF" bt) strlist))
	  )
;	  (push (reverse elm) strlist)
	(when (= bt *RT_ENDSTR*)
	  (push "RT_ENDSTR" strlist)
	  (return (push (list "RT_WTF" (rd-u2 str-in)) strlist))))))

(defun f006 (str-in)
  (let ((bt) (xy))
	(setq elm '("RT_BOUNDARY"))
	(push (list "RT_WTF" (rd-u2 str-in)) elm)
	(loop
	  (setq bt (rd-u2 str-in))
	  (cond
		;((= bt *RT_LAYER*) (push (list "RT_LAYER" (logior (rd-u4 str-in))) elm))
		((= bt *RT_LAYER*) (progn
							 (push (list "RT_LAYER" (rd-u2 str-in)) elm)
							 (push (list "RT_WTF" (rd-u2 str-in)) elm)))
		;((= bt *RT_DATATYPE*) (push (list "RT_DATATYPE" (logior (rd-u4 str-in))) elm))
		((= bt *RT_DATATYPE*) (progn
								(push (list "RT_DATATYPE" (rd-u2 str-in)) elm)
								(push (list "RT_WTF" (rd-u2 str-in)) elm)))
		((= bt *RT_XY*) (push (list "RT_XY" (rd-dt-bitarray1 str-in '())
									;(list "RT_WTF" #x00)
									) elm))
		;((= bt *RT_XY*) (progn
		;				  (push (list "RT_XY" (rd-dt-bitarray1 str-in '())) elm)
		;				  (push (rd-u2 str-in) elm)))
		)
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(push (list "RT_WTF" (rd-u2 str-in)) elm)
		(return elm)))))

(defun f016 (str-in)
  (let ((bt) (xy))
	(setq elm '("RT_PATH"))
	(push (list "RT_WTF" (rd-u2 str-in)) elm)
	(loop
	  (setq bt (rd-u2 str-in))
	  (cond
		;((= bt *RT_LAYER*) (push (list "RT_LAYER" (logior (rd-u4 str-in))) elm))
		((= bt *RT_LAYER*) (progn
							 (push (list "RT_LAYER" (rd-u2 str-in)) elm)
							 (push (list "RT_WTF" (rd-u2 str-in)) elm)))
		;((= bt *RT_DATATYPE*) (push (list "RT_DATATYPE" (logior (rd-u4 str-in))) elm))
		((= bt *RT_DATATYPE*) (progn
								(push (list "RT_DATATYPE" (rd-u2 str-in)) elm)
								(push (list "RT_WTF" (rd-u2 str-in)) elm)))
		;((= bt *RT_PATHTYPE*) (push (list "RT_PATHTYPE" (logior (rd-u4 str-in))) elm))
		((= bt *RT_PATHTYPE*) (progn
								(push (list "RT_PATHTYPE" (rd-u2 str-in)) elm)
								(push (list "RT_WTF" (rd-u2 str-in)) elm)))
		((= bt *RT_WIDTH*) (progn
							 (push (list "RT_WIDTH" (logior (rd-u4 str-in))) elm)
							 (push (list "RT_WTF" (rd-u2 str-in)) elm)))
		((= bt *RT_XY*) (push (list "RT_XY" (rd-dt-bitarray1 str-in '())) elm))
		;((= bt *RT_XY*) (progn
		;				  (push (list "RT_XY" (rd-dt-bitarray1 str-in '())) elm)
		;				  (push (rd-u2 str-in) elm)))
		)
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(push (list "RT_WTF" (rd-u2 str-in)) elm)
		(return elm)))))

(defun f026 (str-in)
  (let ((bt) (xy))
	(setq elm '( "RT_SREF"))
	(push (list "RT_WTF" (rd-u2 str-in)) elm)
	(loop
	  (setq bt (rd-u2 str-in))
	  (cond
		((= bt *RT_SNAME*) (progn
							 ;(push (list "RT_SNAME" (rd-dt-string3 str-in "")) elm)
							 (push (list "RT_SNAME" (rd-dt-string6 str-in)) elm)
							 ;(push (list "RT_SNAME" (rd-dt-string4 str-in)) elm)
							 ;(push (list "RT_WTF" (rd-u2 str-in)) elm)
							 (push (list "RT_WTF" #x0006) elm)
							 ))
		;((= bt *RT_STRANS*) (push (list "RT_STRANS" (logior (rd-u4 str-in))) elm))
		((= bt *RT_STRANS*) (progn
							  (push (list "RT_STRANS" (rd-u2 str-in)) elm)
							  (push (list "RT_WTF" (rd-u2 str-in)) elm)))
		;((= bt *RT_PATHTYPE*) (push (list "RT_PATHTYPE" (logior (rd-u4 str-in))) elm))
		((= bt *RT_PATHTYPE*) (progn
								(push (list "RT_PATHTYPE" (rd-u2 str-in)) elm)
								(push (list "RT_WTF" (rd-u2 str-in)) elm)))
		((= bt *RT_ANGLE*) (progn
							 (push (list "RT_ANGLE" (r642d (rd-u8 str-in))) elm)
							 (push (list "RT_WTF" (rd-u2 str-in)) elm)))
		((= bt *RT_XY*) (push (list "RT_XY" (rd-dt-bitarray1 str-in '())) elm))
		;((= bt *RT_XY*) (progn
		;				  (push (list "RT_XY" (rd-dt-bitarray1 str-in '())) elm)
		;				  (push (rd-u2 str-in) elm)))
		)
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(push (list "RT_WTF" (rd-u2 str-in)) elm)
		(return elm)))))

(defun f036 (str-in)
  (let ((bt))
	(setq elm '("RT_AREF"))
	(push (list "RT_WTF" (rd-u2 str-in)) elm)
	(loop
	  (setq bt (rd-u2 str-in))
	  (cond
		((= bt *RT_SNAME*) (progn
							 ;(push (list "RT_SNAME" (rd-dt-string3 str-in "")) elm)
							 (push (list "RT_SNAME" (rd-dt-string6 str-in)) elm)
							 ;(push (list "RT_WTF" (rd-u2 str-in)) elm)))
							 (push (list "RT_WTF" #x0006) elm)
							 ))
		;((= bt *RT_SNAME*) (push (list "RT_SNAME" (rd-dt-string3 str-in "")) elm))
		;((= bt *RT_STRANS*) (push (list "RT_STRANS" (logior (rd-u4 str-in))) elm))
		;((= bt *RT_STRANS*) (push (list "RT_STRANS" (rd-u2 str-in)) elm))
		;((= bt *RT_STRANS*) (push (list "RT_STRANS" (rd-u2 str-in)) elm))
		((= bt *RT_STRANS*) (progn
							  (push (list "RT_STRANS" (rd-u2 str-in)) elm)
							  (push (list "RT_WTF" (rd-u2 str-in)) elm)))
		;((= bt *RT_ANGLE*) (push (list "RT_ANGLE" (r642d (rd-u8 str-in))) elm))
		((= bt *RT_ANGLE*) (progn
							 (push (list "RT_ANGLE" (r642d (rd-u8 str-in))) elm)
							 (push (list "RT_WTF" (rd-u2 str-in)) elm)))
		((= bt *RT_COLROW*) (push (list "RT_COLROW" (rd-dt-bitarray str-in 4 2)) elm))
;		((= bt *RT_XY*) (progn
;						  (push (list "RT_XY"
;									  ) elm)
;						  (push "RT_ENDEL" elm)
;						  (return elm)))
		((= bt *RT_XY*) (push (list "RT_XY" (rd-dt-bitarray1 str-in '())) elm))
		;((= bt *RT_XY*) (progn
		;				  (push (list "RT_XY" (rd-dt-bitarray1 str-in '())) elm)
		;				  (push (rd-u2 str-in) elm)))
		)
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(push (list "RT_WTF" (rd-u2 str-in)) elm)
		(return elm)))))

(defun f046 (str-in)
  (let ((bt))
	(setq elm '("RT_TEXT"))
	(push (list "RT_WTF" (rd-u2 str-in)) elm)
	(loop
	  (setq bt (rd-u2 str-in))
	  (cond
		((= bt *RT_LAYER*) (progn
							 (push (list "RT_LAYER" (rd-u2 str-in)) elm)
							 (push (list "RT_WTF" (rd-u2 str-in)) elm)))
		;((= bt *RT_LAYER*) (push (list "RT_LAYER" (logior (rd-u4 str-in))) elm))
		;((= bt *RT_LAYER*) (push (list "RT_LAYER" (rd-u2 str-in)) elm))
		;((= bt *RT_DATATYPE*) (push (list "RT_DATATYPE" (logior (rd-u4 str-in))) elm))
		;((= bt *RT_DATATYPE*) (progn
		;						(push (list "RT_DATATYPE" (rd-u2 str-in)) elm)
		;						(push (list "RT_WTF" (rd-u2 str-in)) elm)))
		;((= bt *RT_DATATYPE*) (push (list "RT_DATATYPE" (rd-u2 str-in)) elm))
		((= bt *RT_TEXTTYPE*) (progn
								(push (list "RT_TEXTTYPE" (rd-u2 str-in)) elm)
								(push (list "RT_WTF" (rd-u2 str-in)) elm)))
		;((= bt *RT_TEXTTYPE*) (push (list "RT_TEXTTYPE" (logior (rd-u4 str-in))) elm))
		;((= bt *RT_TEXTTYPE*) (push (list "RT_TEXTTYPE" (rd-u2 str-in)) elm))
		((= bt *RT_PRESENTATION*) (progn
									(push (list "RT_PRESENTATION" (rd-u2 str-in)) elm)
									(push (list "RT_WTF" (rd-u2 str-in)) elm)))
		((= bt *RT_STRANS*) (progn
							  (push (list "RT_STRANS" (rd-u2 str-in)) elm)
							  (push (list "RT_WTF" (rd-u2 str-in)) elm)))
		;((= bt *RT_STRANS*) (push (list "RT_STRANS" (logior (rd-u4 str-in))) elm))
		;((= bt *RT_STRANS*) (push (list "RT_STRANS" (rd-u2 str-in)) elm))
		((= bt *RT_MAG*) (progn
							 (push (list "RT_MAG" (r642d (rd-u8 str-in))) elm)
							 (push (list "RT_WTF" (rd-u2 str-in)) elm)))
		;((= bt *RT_MAG*) (push (list "RT_MAG" (r642d (rd-u8 str-in))) elm))
		((= bt *RT_ANGLE*) (progn
							 (push (list "RT_ANGLE" (r642d (rd-u8 str-in))) elm)
							 (push (list "RT_WTF" (rd-u2 str-in)) elm)))
		;((= bt *RT_ANGLE*) (push (list "RT_ANGLE" (r642d (rd-u8 str-in))) elm))
		((= bt *RT_XY*) (push (list "RT_XY" (rd-dt-bitarray1 str-in '())) elm))
		;((= bt *RT_XY*) (progn
		;				  (push (list "RT_XY" (rd-dt-bitarray1 str-in '())) elm)
		;				  (push (rd-u2 str-in) elm)))
		((= bt *RT_STRING*) (progn
							  ;(push (list "RT_STRING" (rd-dt-string3 str-in "")) elm)
							  ;(push (list "RT_WTF" (rd-u2 str-in)) elm)))
							  (push (list "RT_STRING" (rd-dt-string4 str-in)) elm)
							  (push (list "RT_WTF" #x0004) elm)))
	  )
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(push (list "RT_WTF" (rd-u2 str-in)) elm)
		(return elm)))))

(defun f056 (str-in)
  (let ((bt))
	(setq elm '("RT_NODE"))
	(loop
	  (setq bt (rd-u2 str-in))
	  (when (= bt *RT_ENDEL*)
		(push "RT_ENDEL" elm)
		(push (rd-u2 str-in) elm)
		(return elm)))))

(defun f066 (str-in)
  (let ((bt))
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
