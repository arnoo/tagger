(defpackage :tagger
    (:use     #:cl #:anaphora #:clutch))

(in-package :tagger)

(defstruct col
  name 
  root-dir
  preset
  custom-extensions)

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
        (sort result #'> :key #'file-date))))
        
(defun update-master-index (col)
  (let ((all-files (list-all-files col)))
    (loop with file = (pop all-files)
          do (file-id col file))))

(defun rebuild-indexes (col)
  )

(defun file-date (file)
  (let ((fdate (file-write-date file)))
    (if (in (list ".jpg" ".jpeg") (extension file))
        (handler-bind ((ZPB-EXIF:INVALID-EXIF-STREAM [return-from file-date fdate]))
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
  (gulp file :binary t))

(defun bungulp (file data)
  (ungulp file data :binary t))

(defmacro with-open-bitfile (params &body body)
   `(with-open-file (,@params :element-type 'bit) ,@body))

(defun col-data-dir (col)
  (str (col-root-dir col)
       "/.tagger/"
       (col-name col)))

(defun col-tag-file (col tag)
  (str (col-data-dir col) "/tags/" tag) "")

(defun tag-exists (col tag)
  (probe-file (col-tag-file col tag)))

(defun assert-tag-exists (col tag)
  (unless (tag-exists col tag)
    (error "Unknown tag")))

(defun make-tag (col tag)
  (with-open-file (f (col-tag-file col tag) :direction :output)
    (write-sequence nil f)))

(defun tag (col file tag)
  "Add <tag> to <file> in col <col>"
  (unless (tag-exists col tag)
    (make-tag col tag))
  (let* ((fid (file-id col file))
         (tag-file (col-tag-file col tag))
         (tag-file-size (if (probe-file tag-file) (filesize tag-file) 0)))
    (with-open-bitfile (f tag-file :if-exists :overwrite)
        (when (< tag-file-size
                 fid)
          (write-sequence (make-array (- fid tag-file-size) :element-type 'bit :initial-element 0)
                          f))
        (file-position f fid)
        (write-byte 1 f))))

(defun untag (col file tag)
  "Remove <tag> for <file>"
  (assert-tag-exists col tag)
  (let* ((fid (file-id col file))
         (tag-file (col-tag-file col tag))
         (tag-file-size (if (probe-file tag-file) (filesize tag-file) 0)))
        (when (>= tag-file-size
                  fid)
          (with-open-bitfile (f tag-file :direction :output :if-exists :overwrite)
            (file-position f fid)
            (write-byte 0 f)))))

(defun file-id-file (col file)
    (str (col-data-dir col) "/fileids/" (~s "/\\//%%/g" file)))

(defun file-id (col file)
  (awith (file-id-file col file)
    (if (probe-file it)
        (gulp it :binary t)
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

(defmacro with-tagfile-seq ((seq col tag maxid offset limit) &body body)
   `(let ((,seq (make-array ,maxid :element-type 'bit :initial-element 0)))
      (with-open-bitfile (f (col-tag-file ,col ,tag))
        (awhen ,offset (file-position f it))
        (read-sequence ,seq f :end ,limit)
        ,@body)))

(defun list-files (col &key +tags -tags (offset 0) limit)
  "List files in <col> matching <+tags> and not <-tags>"
  (let* ((maxid (max (col-max-id col) (if limit (+ offset limit) 0)))
         (okids (make-array (if limit
                                (min (- maxid offset) limit)
                                (- maxid offset))
                            :element-type 'bit)))
    (loop for tag in +tags
          do (with-tagfile-seq (seq col tag maxid offset limit)
               (bit-and okids seq)))
    (loop for tag in -tags
          do (with-tagfile-seq (seq col tag maxid offset limit)
               (bit-and okids (bit-not seq))))
    (setf okids (delete 0 okids))
    (mapcar [probe-file (gulp (str (col-data-dir col) "/filenames/" _))] okids)))

(defun init-col (col)
  (mkdir (str (col-data-dir col)))
  (mkdir (str (col-data-dir col) "/tags"))
  (mkdir (str (col-data-dir col) "/filenames"))
  (mkdir (str (col-data-dir col) "/fileids")))

