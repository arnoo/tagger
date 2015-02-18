(defpackage :tagger
    (:use     #:cl #:clutch)
    (:shadowing-import-from #:clutch)
    (:export  #:tag #:untag #:list-files #:id-file #:extension
              #:make-col #:col-extensions #:col-name #:col-root-dir #:col-preset #:col-custom-extensions
              #:list-tags
              #:update-master-index #:rebuild-index))

(in-package :tagger)

(defclass col ()
  ((name               :accessor col-name               :initarg :name)
   (root-dir           :accessor col-root-dir           :initarg :root-dir)
   (preset             :accessor col-preset             :initarg :preset :initform nil)
   (custom-extensions  :accessor col-custom-extensions  :initarg :custom-extensions :initform nil)))

(defun bytes-to-integer (bytes)
  (reduce #'(lambda (a b) (+ (ash a 8) b)) bytes))

(defun bits-to-byte (bitseq)
  (reduce #'(lambda (a b) (+ (ash a 1) b))
          (if (= (length bitseq) 8)
              bitseq
              (concatenate 'list bitseq (x #*1
                                           (- 8 (length bitseq)))))))

(defun integer-to-bytes (integer)
  (if (zerop integer)
      '(0)
      (reverse
        (loop for index from 0 by 8
              while (> (ash integer (- index)) 0)
              collect (ldb (byte 8 index) integer)))))

(defun bitseq-to-byteseq (bitseq)
  (loop for i from 0 to (floor (length bitseq) 8)
        collect (bits-to-byte {bitseq (* i 8) (min (* (+ i 1) 8) (length bitseq))})))
 
(defmethod col-extensions ((col col))
  (append (case (col-preset col)
            (:IMG (list "jpg" "jpeg" "png" "ico" "bmp" "xpm" "gif"))
            (:VID (list "mpg" "mkv" "avi" "mov" "m4v" "flv"))
            (:MEDIA (flatten (mapcar #'col-extensions (list :IMG :VID)))))
          (col-custom-extensions col)))

(defun dir-list (dir)
  (handler-bind ((SB-INT:C-STRING-DECODING-ERROR [return-from dir-list nil]))
    (directory (str dir "/*.*"))))

(defmethod list-all-files ((col col) &optional dir)
  (let ((result (flatten (loop for node in (dir-list (or dir (col-root-dir col)))
                               unless (hidden node)
                                 if (probe-dir node)
                                 collect (list-all-files col node)
                                 else when (in (col-extensions col) (extension node))
                                 collect node))))
    (if dir
        result
        (stable-sort (remove nil result) #'> :key #'file-date))))
        
(defmethod update-master-index ((col col))
  (let ((all-files (list-all-files col)))
    (loop for file = (pop all-files)
          while file
          do (file-id col file))))

(defmethod rebuild-index ((col col))
  )

(defun file-date (file)
  (let ((fdate (file-write-date file)))
    (if (in (list "jpg" "jpeg") (extension file))
        (handler-bind ((ZPB-EXIF:INVALID-JPEG-STREAM [return-from file-date fdate])
                       (ZPB-EXIF:INVALID-EXIF-STREAM [return-from file-date fdate]))
          (awith (substitute #\- #\: (zpb-exif:exif-value :DateTimeOriginal (zpb-exif:make-exif file)) :count 2)
             (if (and it (string= it "0000" :end1 4))
                 fdate
                 (strtout it))))
        fdate)))

(defun hidden (path)
  (setf path (string-right-trim "/" (str path)))
  (char= {path (+ (or (position #\/ path :from-end t)
                      -1)
                  1)}
         #\.))

(defun extension (file)
  (setf file (str file))
  (lc {file (or (+ 1 (position #\. file :from-end t))
                0)
            -1}))

(defun bgulp (file)
  (bytes-to-integer (gulp file :binary t)))

(defun bungulp (file int)
  (ungulp file (integer-to-bytes int) :binary t :if-exists :supersede))

(defmacro with-open-binfile (params &body body)
   `(with-open-file (,@params :element-type '(unsigned-byte 8)) ,@body))

(defmethod col-data-dir ((col col))
  (str (col-root-dir col)
       "/.tagger/"
       (col-name col)))

(defmethod col-tag-file ((col col) tag)
  (str (col-data-dir col) "/tags/" (~s "/\\//%%/g" tag)))

(defmethod tag-exists ((col col) tag)
  (probe-file (col-tag-file col tag)))

(defmethod assert-tag-exists ((col col) tag)
  (unless (tag-exists col tag)
    (error "Unknown tag")))

(defun write-bit (value file position)
  "Writes bit with value <value> at <position> in <file>. Since unwritten bits are read as zeros, if position is more than file length, we don't need to write the bit"
  (let* ((file-size (if (probe-file file) (filesize file) 0)))
    (when (and (= value 0)
               (< file-size (ceiling (+ position 1) 8)))
      (return-from write-bit nil))
    (with-open-binfile (f file :direction :io
                               :if-exists :overwrite
                               :if-does-not-exist :create)
       (when (< file-size
                (ceiling (+ position 1) 8))
         (file-position f file-size)
         (write-sequence (make-array (- (ceiling (+ position 1) 8) file-size)
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)
                          f))
       (file-position f (floor position 8))
       (let ((current-byte (read-byte f))
             (byte-index (mod position 8)))
         (when (xor (logbitp byte-index current-byte)
                    (= value 1))
             (file-position f (floor position 8))
             (write-byte (dpb value (byte 1 byte-index) current-byte) f))))))

(defmethod tag ((col col) file tag)
  "Add <tag> to <file> in col <col>"
  (let* ((fid (file-id col file))
         (tag-file (col-tag-file col tag)))
    (write-bit 1 tag-file fid)))

;TODO: Check that file exists
(defmethod untag ((col col) file tag)
  "Remove <tag> for <file>"
  (assert-tag-exists col tag)
  (let* ((fid (file-id col file))
         (tag-file (col-tag-file col tag)))
    (write-bit 0 tag-file fid)))

(defun read-bit (file position)
  (with-open-binfile (f file)
    (file-position f (floor position 8))
    (if (logbitp (mod position 8) (read-byte f))
        1
        0)))

(defmethod tags ((col col) file)
  "List the tags for <file> in <col>"
  (let ((fid (file-id col file)))
    (loop for tag-file in (ls (str (col-data-dir col) "/tags/"))
          when (= 1 (read-bit tag-file fid))
          collect (tagfile-tag tag-file))))

(defmethod file-id-file ((col col) file)
    (str (col-data-dir col) "/fileids/" (~s "/\\//%%/g" file)))

(defmethod file-id ((col col) file)
  (awith (file-id-file col file)
    (if (probe-file it)
        (bgulp it)
        (make-file-id col file))))

(defmethod make-file-id ((col col) file &key noprobe)
  (unless (or noprobe
              (probe-file file))
    (error (str "File not found : " file)))
  (let* ((maxfile (str (col-data-dir col) "/max_id"))
         (id (if (probe-file maxfile)
                 (+ (bgulp maxfile) 1)
                 0)))
    (bungulp (file-id-file col file) id)
    (bungulp maxfile id)
    (ungulp (str (col-data-dir col) "/filenames/" id)
            (str file))
    id))

(defmethod col-max-id ((col col))
  (let ((maxfile (str (col-data-dir col) "/max_id")))
     (if (probe-file maxfile)
         (bgulp maxfile)
         0)))

(defun byte-to-bits (byte)
  (awith (make-array 8 :element-type 'bit :initial-element 0)
    (loop for i from 0 to 7
          when (logbitp i byte)
          do (setf (aref it i) 1))
    it))

(defmacro read-bitseq (seq stream &key end)
  `(let* ((tempseq (make-array (ceiling (array-total-size ,seq) 8)
                              :element-type '(unsigned-byte 8)
                              :initial-element 0))
          (bytes-read (read-sequence tempseq ,stream :end (if ,end (ceiling ,end 8) nil))))
     (loop for i from 0 below bytes-read
           do (setf (subseq ,seq (* i 8) (* (+ i 1) 8))
                    (byte-to-bits (elt tempseq i))))))

(defmacro with-tagfile-seq ((seq col tag offset limit) &body body)
  `(with-file-seq (,seq (col-tag-file ,col ,tag) offset limit)
      ,@body))

(defmacro with-file-seq ((seq file offset limit) &body body)
   `(let ((,seq (make-array ,limit :element-type 'bit :initial-element 0)))
      (with-open-binfile (f ,file)
        (file-position f (floor offset 8))
        (read-bitseq ,seq f :end ,limit)
        ,@body)))

(defmacro with-tag-and-subtags-seq ((seq col tag offset limit) &body body)
  `(with-tagfile-seq (,seq ,col ,tag ,offset ,limit)
     (loop for subtag in (subtags ,col ,tag) 
           do (with-tagfile-seq (subseq ,col subtag ,offset ,limit)
                 (setf ,seq (bit-ior subseq ,seq))))
     ,@body))

(defun tagfile-tag (tagfile)
  (when (stringp tagfile)
    (setf tagfile (parse-namestring tagfile)))
  (~s "/%%/\\//g" (str (pathname-name tagfile))))

(defmethod subtags ((col col) tag)
  (mapcar #'tagfile-tag (directory (str (col-tag-file col tag) "%%*"))))

(defmethod list-files ((col col) &key +tags -tags (offset 0) limit format)
  "List files in <col> matching <+tags> and not <-tags>"
  (declare (optimize debug))
  (let ((maxid (col-max-id col)))
    (when (or (not limit)
              (< (- (+ maxid 1) offset) limit))
        (setf limit (- (+ maxid 1) offset))))
  (let ((okids (make-array limit
                           :element-type 'bit
                           :initial-element 1)))
    (loop for tag in +tags
          do (with-tag-and-subtags-seq (seq col tag offset limit)
               (setf okids (bit-and okids seq))))
    (loop for tag in -tags
          do (with-tagfile-seq (seq col tag offset limit)
               (setf okids (bit-and okids (bit-not seq)))))
    (ecase format
      ((nil :ids-only)
        (loop for i from 0 below (length okids)
              for fname = (and (= 1 (aref okids i))
                    (id-file col i))
              when fname
              collect (if (eq format :ids-only) i fname)))
      (:bitseq
         (loop for i from 0 below (length okids)
               when (and (= 1 (aref okids i))
                         (not (id-file col i)))
               do (setf (aref okids i) 0))
         okids))))

(defmethod id-file ((col col) id)
   (probe-file (gulp (str (col-data-dir col) "/filenames/" id))))

(defun make-col (&rest args)
  (awhen (getf args :custom-extensions)
    (assert (not (~ "/^\\./" it))))
  (awith
    (apply #'make-instance (cons 'col args))
    (init-col it)
    it))
                   
(defmethod init-col ((col col))
  (mkdir (str (col-root-dir col) "/.tagger"))
  (mkdir (col-data-dir col))
  (mkdir (str (col-data-dir col) "/tags"))
  (mkdir (str (col-data-dir col) "/filenames"))
  (mkdir (str (col-data-dir col) "/fileids"))
  (ungulp (str (col-data-dir col)
               "/config")
          (with-output-to-string (s)
             (write (list :preset (col-preset col) :custom-extensions (col-custom-extensions col)) :stream s))))

(defun load-col (root-dir name)
  (let ((col (make-instance 'col
                :root-dir root-dir
                :name name)))
     (destructuring-bind (&key preset custom-extensions)
                         (read-from-string (gulp (str (col-data-dir col) "/config")))
       (setf (col-preset col) preset)
       (setf (col-custom-extensions col) custom-extensions))
     col))

(defun list-cols (&key dir)
  (unless dir
    (setf dir (env "PWD"))))

(defun find-col (&key dir)
  (unless dir
    (setf dir (env "PWD")))
  (let ((tdir (str dir "/.tagger")))
     (if (probe-dir tdir)
         (let ((cols (ls tdir)))
           (cond ((> (length cols) 1)
                    (error (str "Multiple collections found, please specify one of : " (join ", " (list-cols :dir dir)))))
                 ((= (length cols) 0)
                    (find-col :dir dir))
                 (t (load-col dir col))))
         (find-col :dir dir))))

(defun parent-dir (dir)
  (if (string= (str dir) "/")
      nil
      (awith (remove "" (split "/" (str dir)))
        (str "/" (join "/" {it 0 -2})))))
        
(defun list-tags (&key start col)
  (unless col
    (setf col (find-col)))
  (awith (ls (str (col-data-dir col) "/tags/"))
    (if start
        (remove-if-not [in _ start :at 0] it)
        it)))
