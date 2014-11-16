(defpackage :tagger
    (:use     #:cl #:anaphora #:clutch))

(in-package :tagger)

(defstruct col
  name 
  root-dir
  preset
  custom-extensions)

(defun bytes-to-integer (bytes)
  (reduce #'(lambda (a b) (+ (ash a 8) b)) bytes))

(defun integer-to-bytes (integer)
  (if (zerop integer)
      '(0)
      (reverse
        (loop for index from 0 by 8
              while (> (ash integer (- index)) 0)
              collect (ldb (byte 8 index) integer)))))
 
(defun col-extensions (col)
  (case (col-preset col)
    (:IMG (list ".jpg" ".jpeg" ".png" ".ico" ".bmp" ".xpm" ".gif"))
    (:VID (list ".mpg" ".mkv" ".avi" ".mov" ".m4v" ".flv"))
    (:MEDIA (flatten (mapcar #'col-extensions (list :IMG :VID))))
    (otherwise (col-custom-extensions col))))

(defun dir-list (dir)
  (handler-bind ((SB-INT:C-STRING-DECODING-ERROR [return-from dir-list nil]))
    (directory (str dir "/*.*"))))

(defun list-all-files (col &optional dir)
  (let ((result (flatten (loop for node in (dir-list (or dir (col-root-dir col)))
                               unless (hidden node)
                                 if (probe-dir node)
                                 collect (list-all-files col node)
                                 else when (in (col-extensions col) (extension node))
                                 collect node))))
    (if dir
        result
        (sort (remove nil result) #'> :key #'file-date))))
        
(defun update-master-index (col)
  (let ((all-files (list-all-files col)))
    (loop for file = (pop all-files)
          while all-files
          do (file-id col file))))

(defun rebuild-indexes (col)
  )

(defun file-date (file)
  (let ((fdate (file-write-date file)))
    (if (in (list ".jpg" ".jpeg") (extension file))
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
  (lc {file (or (position #\. file :from-end t)
                0)
            -1}))

(defun bgulp (file)
  (bytes-to-integer (gulp file :binary t)))

(defun bungulp (file int)
  (ungulp file (integer-to-bytes int) :binary t :if-exists :supersede))

(defmacro with-open-binfile (params &body body)
   `(with-open-file (,@params :element-type '(unsigned-byte 8)) ,@body))

(defun col-data-dir (col)
  (str (col-root-dir col)
       "/.tagger/"
       (col-name col)))

(defun col-tag-file (col tag)
  (str (col-data-dir col) "/tags/" tag))

(defun tag-exists (col tag)
  (probe-file (col-tag-file col tag)))

(defun assert-tag-exists (col tag)
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

(defun tag (col file tag)
  "Add <tag> to <file> in col <col>"
  (let* ((fid (file-id col file))
         (tag-file (col-tag-file col tag)))
    (write-bit 1 tag-file fid)))

(defun untag (col file tag)
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

(defun tags (col file)
  "List the tags for <file> in <col>"
  (let ((fid (file-id file)))
    (loop for tag-file in (ls (str (col-root-dir col) "/tags/"))
          when (= 1 (read-bit tag-file fid))
          collect (tagfile-tag tag-file))))

(defun file-id-file (col file)
    (str (col-data-dir col) "/fileids/" (~s "/\\//%%/g" file)))

(defun file-id (col file)
  (awith (file-id-file col file)
    (if (probe-file it)
        (bgulp it)
        (make-file-id col file))))

(defun make-file-id (col file)
  (let* ((maxfile (str (col-data-dir col) "/max_id"))
         (id (if (probe-file maxfile)
                 (+ (bgulp maxfile) 1)
                 0)))
    (bungulp (file-id-file col file) id)
    (bungulp maxfile id)
    (ungulp (str (col-data-dir col) "/filenames/" id)
            (str file))
    id))

(defun col-max-id (col)
  (let ((maxfile (str (col-data-dir col) "/max_id")))
     (if (probe-file maxfile)
         (bgulp maxfile)
         0)))

(defun byte-to-bits (byte)
  (awith (make-array 8 :element-type 'bit)
    (loop for i from 0 to 7
       do (setf (elt it i)
                (if (logbitp i byte) 1 0)))
    it))

(defmacro read-bitseq (seq stream &key end)
  `(let ((tempseq (make-array (ceiling (/ (array-total-size seq) 8))
                              :element-type '(unsigned-byte 8)
                              :initial-element 0)))
     (read-sequence tempseq ,stream :end (ceiling (/ 8 ,end)))
     (loop for i from 0 below ,end
           do (setf (subseq ,seq (* i 8) (+ (* i 8) 1))
                    (byte-to-bits (elt tempseq i))))))

(defmacro with-tagfile-seq ((seq col tag maxid offset limit) &body body)
  `(with-file-seq (,seq (col-tag-file ,col ,tag) maxid offset limit)
      ,@body))

(defmacro with-file-seq ((seq file maxid offset limit) &body body)
   `(let ((,seq (make-array ,maxid :element-type 'bit :initial-element 0)))
      (with-open-binfile (f ,file)
        (awhen ,offset (file-position f (/ it 8)))
        (read-bitseq ,seq f :end ,limit)
        ,@body)))

(defmacro with-tag-and-subtags-seq ((seq col tag maxid offset limit) &body body)
  `(with-tagfile-seq (,seq ,col ,tag ,maxid ,offset ,limit)
     (loop for subtag in (subtags ,col ,tag) 
           do (with-tagfile-seq (subseq ,col subtag ,maxid ,offset ,limit)
                 (bit-or subseq ,seq)))
     ,@body))

(defun tagfile-tag (tagfile)
  (when (stringp tagfile)
    (setf tagfile (namestring tagfile)))
  (~s "/%%/\\//g" (str (pathname-name tagfile))))

(defun subtags (col tag)
  (mapcar #'tagfile-tag (directory (str (col-tag-file col tag) "%%*"))))

(defun list-files (col &key +tags -tags (offset 0) limit)
  "List files in <col> matching <+tags> and not <-tags>"
  (let* ((maxid (max (col-max-id col) (if limit (+ offset limit) 0)))
         (okids (if +tags (make-array (if limit
                                          (min (- maxid offset) limit)
                                          (- maxid offset))
                                     :element-type 'bit
                                     :initial-element 1))))
    (loop for tag in +tags
          do (with-tag-and-subtags-seq (seq col tag maxid offset limit)
               (bit-and okids seq)))
    (loop for tag in -tags
          do (with-tagfile-seq (seq col tag maxid offset limit)
               (bit-and okids (bit-not seq))))
    (setf okids (delete 0 okids))
    (remove nil (mapcar [probe-file (gulp (str (col-data-dir col) "/filenames/" _))] okids))))

(after make-col
       (init-col


(defun init-col (col)
  (mkdir (str (col-data-dir col)))
  (mkdir (str (col-data-dir col) "/tags"))
  (mkdir (str (col-data-dir col) "/filenames"))
  (mkdir (str (col-data-dir col) "/fileids")))

