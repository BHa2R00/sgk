(defvar *RT_BGNLIB*			#x0102)
(defvar *RT_HEADER*			#x0002)
(defvar *RT_LIBNAME*		#x0206)
(defvar *RT_UNITS*			#x0305)
(defvar *RT_ENDLIB*			#x0400)
(defvar *RT_BGNSTR*			#x0502)
(defvar *RT_STRNAME*		#x0606)
(defvar *RT_ENDSTR*			#x0700)
(defvar *RT_BOUNDARY*		#x0800)
(defvar *RT_PATH*			#x0900)
(defvar *RT_SREF*			#x0A00)
(defvar *RT_AREF*			#x0B00)
(defvar *RT_TEXT*			#x0C00)
(defvar *RT_LAYER*			#x0D02)
(defvar *RT_DATATYPE*		#x0E02)
(defvar *RT_WIDTH*			#x0F03)
(defvar *RT_XY*				#x1003)
(defvar *RT_ENDEL*			#x1100)
(defvar	*RT_SNAME*			#x1206)
(defvar	*RT_COLROW*			#x1302)
(defvar	*RT_TEXTNODE*		#x1400)
(defvar	*RT_NODE*			#x1500)
(defvar	*RT_TEXTTYPE*		#x1602)
(defvar	*RT_PRESENTATION*	#x1701)
(defvar	*RT_SPACING*		#x1800)
(defvar *RT_STRING* 		#x1906)
(defvar *RT_STRANS*			#x1A01)
(defvar *RT_MAG*			#x1B05)
(defvar *RT_ANGLE*			#x1C05)
(defvar *RT_UINTEGER*		#x1D00)
(defvar *RT_USTRING*		#x1E00)
(defvar *RT_REFLIB*			#x1F06)
(defvar *RT_FONTS*			#x2006)
(defvar *RT_PATHTYPE*		#x2102)
(defvar *RT_GENERATIONS*	#x2202)
(defvar *RT_ATTRTABLE*		#x2306)
(defvar *RT_STYPTABLE*		#x2406)
(defvar *RT_STRTYPE*		#x2502)
(defvar *RT_ELFLAGS*		#x2601)
(defvar *RT_ELKEY*			#x2703)
(defvar *RT_LINKTYPE*		#x2800)
(defvar *RT_LINKKEYS*		#x2900)
(defvar *RT_NODETYPE*		#x2A02)
(defvar *RT_PROPATTR*		#x2B02)
(defvar *RT_PROPVALUE*		#x2C06)
(defvar *RT_BOX*			#x2D00)
(defvar *RT_BOXTYPE*		#x2E02)
(defvar *RT_PLEX*			#x2F03)
(defvar *RT_BGNEXTN*		#x3003)
(defvar *RT_ENDEXTN*		#x3103)
(defvar *RT_TAPENUM*		#x3202)
(defvar *RT_TAPECODE*		#x3302)
(defvar *RT_STRCLASS*		#x3401)
(defvar *RT_RESERVED*		#x3503)
(defvar *RT_FORMAT*			#x3602)
(defvar *RT_MASK*			#x3706)
(defvar *RT_ENDMASK*		#x3800)
(defvar *RT_LIBDIRSIZE*		#x3902)
(defvar *RT_SRFNAME*		#x3A06)
(defvar	*RT_LIBSECUR*		#x3B02)
(defvar	*DT_NODATA*			#x00)
(defvar	*DT_BITARRAY*		#x01)
(defvar	*DT_SIGNED16*		#x02)
(defvar	*DT_SIGNED32*		#x03)
(defvar	*DT_REAL32*			#x04)
(defvar	*DT_REAL64*			#x05)
(defvar	*DT_STRING*			#x06)
(defvar	*PRES_MASK_H*		#x0003)
(defvar	*PRES_MASK_V*		#x000C)
(defvar	*PRES_MASK_FONT*	#x0030)
(defvar	*PRES_TOP*			#x0)
(defvar	*PRES_MIDDLE*		#x1)
(defvar	*PRES_BOTTOM*		#x2)
(defvar	*PRES_LEFT*			#x0)
(defvar	*PRES_CENTER*		#x1)
(defvar	*PRES_RIGHT*		#x2)
(defvar	*STRANS_ABSMAG*		#x0002)
(defvar	*STRANS_ABSANGLE*	#x0004)
(defvar	*STRANS_REFLECTION*	#x8000)
(defvar	*PATHTYPE_FLUSH*	#x0)
(defvar	*PATHTYPE_ROUND*	#x1)
(defvar	*PATHTYPE_SQUARE*	#x2)
(defvar	*ELFLAG_TEMPLATE*	#x0001)
(defvar	*ELFLAG_EXTERNAL*	#x0002)
(defvar	*PLEX_HEAD*			#x01000000)
(defvar	*PLEX_VALID_BITS*	#x00FFFFFF)

;(defvar *anchor-layer-number* 60)

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
(load-shared-object "./sgk/rw.so")
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

(defun is-name-head? (asii)
  (let ((c (code-char asii)))
  (if 	(or
		(and (char>= c #\A) (char<= c #\Z))
		(and (char>= c #\a) (char<= c #\z))
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

(defun rd-dt-string41 (str-in)
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

(defun rd-dt-string4 (str-in)
  (let ((stri (rd-dt-string41 str-in)))
	(if (is-name-head? (char-code (char stri 0)))
	  stri
	  (subseq stri 1))
	))

(defun rd-dt-string61 (str-in)
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

(defun rd-dt-string6 (str-in)
  (let ((stri (rd-dt-string61 str-in)))
	(if (is-name-head? (char-code (char stri 0)))
	  stri
	  (subseq stri 1))))

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
	  ;(format t "get ~a~%" name)
	  (return str))))

(defun is-boundary? (elm)
  (if (string= (car elm) "RT_BOUNDARY") t nil))

(defun get-boundary (str layer)
  (let ((lst1 (list)))
	(dolist (elm str)
	  (if (listp elm)
		(if (is-boundary? elm)
		  (if (= layer (cadr (cadr elm)))
			(push (cadr (cadddr elm)) lst1) nil)
		  nil) nil))
	lst1))

(defun is-rectangle? (b-xy) (if (= 8 (- (length b-xy) 2)) t nil))

(defun size-of-rectangle (b-xy)
  (let ((size))
	(if (is-rectangle? b-xy)
	  (setf size (list
				   (max (aref b-xy 0) (aref b-xy 2) (aref b-xy 4) (aref b-xy 6))
				   (max (aref b-xy 1) (aref b-xy 3) (aref b-xy 5) (aref b-xy 7))
				   )) nil)
	(make-array '(2) :initial-contents size)))

(defun math-pow (a x)
  (if (= a 0)
	0
	(if (= x 0)
	  1
	  (exp (* x (log a))))))

(defun math-log (a x)
  (/ (log x) (log  a)))

(defun sgk-rd-gds (fi) (f-return-srctree fi))

(defun sgk-wt-gds (fo tree) (sgk-f100 fo (flatten-tree tree)))

(defun sgk-lib (name units)
  (list (list "RT_HEADER" 5)
		(list "RT_BGNLIB" #(117 1 11 16 17 41 117 1 11 16 21 29))
		(list "RT_LIBNAME" name)
		(list "RT_UNITS" 9.999999999999998d-4 9.999999999999999d-10
			  units)
		"RT_ENDLIB"))

(defun sgk-cell (name elms)
  (let ((lst (list (list "RT_STRNAME" name) (list "RT_BGNSTR" #(70 1 1 8 0 0 118 2 2 14 29 35)))))
	(dolist (elm elms)
	  (push elm lst))
	(push "RT_ENDSTR" lst)
	(reverse lst)))

(defun sgk-sref (sname strans angle xy)
  (if (= strans #x4000)
	(sgk-sref1 sname #x8000 (+ 180.0d0 angle) xy)
	(sgk-sref1 sname strans angle xy)))

(defun sgk-sref1 (sname strans angle xy)
  (list "RT_SREF" (list "RT_SNAME" sname)
	(list "RT_STRANS" strans)
	(list "RT_ANGLE" angle)
	(list "RT_XY" (make-array '(2) :initial-contents xy))
	"RT_ENDEL"))

(defun sgk-text (layer typ pre strans mag xy text)
  (list "RT_TEXT"
	(list "RT_LAYER" layer)
	(list "RT_TEXTTYPE" typ)
	(list "RT_PRESENTATION" pre)
	(list "RT_STRANS" strans)
	(list "RT_MAG" mag)
	(list "RT_XY" (make-array '(2) :initial-contents xy))
	(list "RT_STRING" text)
	"RT_ENDEL"))

(defun sgk-boundary (layer datatype xy)
  (list "RT_BOUNDARY"
		(list "RT_LAYER" layer)
		(list "RT_DATATYPE" datatype)
		(list "RT_XY" (make-array (list (length xy)) :initial-contents xy))
		"RT_ENDEL"))

(defun sgk-boundary-rectangle (layer strans x0 y0 w h)
  (cond
	((= strans #x4000) (sgk-boundary-rectangle layer #x0000 x0 y0 (* -1 w) h))
	((= strans #x8000) (sgk-boundary-rectangle layer #x0000 x0 y0 w (* -1 h)))
	(t (sgk-boundary layer 0 (list x0 y0
								   (+ x0 w) y0
								   (+ x0 w) (+ y0 h)
								   x0 (+ y0 h)
								   x0 y0)))))

(defun sgk-ls-cell (lib)
  (dolist (n (get-units lib))
  (format t "~a~%" (get-str-name n))))

(defun sgk-xy2d (x y)
  (make-array '(2) :initial-contents (list x y)))
