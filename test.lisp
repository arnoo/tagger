(defpackage :tagger-test
   (:use :cl :clutch :stefil))

(in-package :tagger-test)

(defvar *tfile* #p"/tmp/test-write-bit")

(defsuite :tagger-bits)
(in-suite :tagger-bits)

(deftest write-zero-to-empty-file ()
  (rm *tfile*)
  (tagger::write-bit 0 *tfile* 0)
  (is (null (probe-file *tfile*)))
  (tagger::write-bit 0 *tfile* 10)
  (is (null (probe-file *tfile*))))

(deftest write-one-to-empty-file ()
  (tagger::write-bit 1 *tfile* 1)
  (is (probe-file *tfile*)))

(deftest edge-bit-positions ()
  (rm *tfile*)
  (tagger::write-bit 1 *tfile* 0)
  (tagger::write-bit 1 *tfile* 7)
  (tagger::write-bit 1 *tfile* 15)
  (is (probe-file *tfile*))
  (is (equal (vector-to-list* (gulp *tfile* :binary t))
             (list 129 128)))
  (tagger::write-bit 1 *tfile* 8)
  (is (equal (vector-to-list* (gulp *tfile* :binary t))
             (list 129 129))))


(deftest bit-overwrite ()
  (rm *tfile*)
  (tagger::write-bit 1 *tfile* 0)
  (tagger::write-bit 1 *tfile* 7)
  (tagger::write-bit 1 *tfile* 8)
  (tagger::write-bit 1 *tfile* 15)
  (is (equal (vector-to-list* (gulp *tfile* :binary t))
             (list 129 129)))
  (tagger::write-bit 0 *tfile* 8)
  (is (equal (vector-to-list* (gulp *tfile* :binary t))
             (list 129 128)))
  (tagger::write-bit 1 *tfile* 8)
  (is (equal (vector-to-list* (gulp *tfile* :binary t))
             (list 129 129)))
  (tagger::write-bit 0 *tfile* 0)
  (tagger::write-bit 0 *tfile* 8)
  (is (equal (vector-to-list* (gulp *tfile* :binary t))
             (list 128 128))))


(deftest faraway-bit ()
  (rm *tfile*)
  (tagger::write-bit 1 *tfile* 0)
  (tagger::write-bit 1 *tfile* 7)
  (tagger::write-bit 1 *tfile* 8)
  (tagger::write-bit 1 *tfile* 15)
  (is (equal (vector-to-list* (gulp *tfile* :binary t))
             (list 129 129)))
  (tagger::write-bit 0 *tfile* 55)
  (is (equal (vector-to-list* (gulp *tfile* :binary t))
             (list 129 129)))
  (tagger::write-bit 1 *tfile* 55)
  (is (equal (vector-to-list* (gulp *tfile* :binary t))
             (append (list 129 129) (x '(0) (- (floor 55 8) 2)) (list 128))))
  (tagger::write-bit 0 *tfile* 55)
  (is (equal (vector-to-list* (gulp *tfile* :binary t))
             (append (list 129 129) (x '(0) (- (floor 55 8) 2)) (list 0)))))

(defsuite :tagger-tags)
(in-suite :tagger-tags)

(defmacro with-test-col ((col) &body body)
  `(unwind-protect 
       (progn
          (mkdir "/tmp/tagger-test-col")
          (loop for i from 1 to 100
            do (ungulp (str "/tmp/tagger-test-col/file" i ".txt") "file contents"))
          (let ((,col (tagger::make-col :root-dir "/tmp/tagger-test-col" :name (str (gensym)) :custom-extensions (list ".txt"))))
            ,@body))
       (rm "/tmp/tagger-test-col" :recursive t)))


(deftest basic-tag-untag ()
  (with-test-col (col)
    (tagger::update-master-index col)
    (awith "/tmp/tagger-test-col/file10"
        (tagger::tag col it "tag1")
        (is (tagger::read-bit (tagger::col-tag-file col "tag1") 1) 1)
        (is (tagger::tags col it) (list "tag1"))
        (tagger::untag col it "tag1")
        (is (tagger::read-bit (tagger::col-tag-file col "tag1") 1) 0)
        (is (null (tagger::tags col it))))))

(deftest list-files ()
  (with-test-col (col)
     (tagger::update-master-index col)
     (let ((file1 #p"/tmp/tagger-test-col/file10")
           (file2 #p"/tmp/tagger-test-col/file22")
           (file3 #p"/tmp/tagger-test-col/file33"))
       (tagger::tag col file1 "tag1")
       (tagger::tag col file2 "tag1")
       (tagger::tag col file3 "tag1")
       (tagger::tag col file1 "tag2")
       (tagger::tag col file2 "tag2/subtag2")
       (tagger::tag col file3 "tag2")
       (tagger::tag col file1 "tag3")
       (tagger::tag col file2 "tag3")
       (tagger::tag col file3 "tag4")

       (is (tagger::list-files col)
           (loop for i from 1 to 100 collect (namestring (str "/tmp/tagger-test-col/file" i ".txt"))))
       (is (tagger::list-files col :+tags '("tag1"))
           '(file1 file2 file3))
       (is (tagger::list-files col :+tags '("tag2"))
           '(file1 file2 file3))
       (is (tagger::list-files col :+tags '("tag2/subtag2"))
           '(file2))
       (is (tagger::list-files col :+tags '("tag3"))
           '(file1 file2))
       (is (tagger::list-files col :+tags '("tag1" "tag2"))
           '(file1 file2 file3))
       (is (tagger::list-files col :+tags '("tag1" "tag2/subtag2"))
           '(file2))
       (is (tagger::list-files col :+tags '("tag4" "tag2/subtag2"))
           '())
       (is (tagger::list-files col :-tags '("tag4"))
           '(file1 file2))
       (is (tagger::list-files col :-tags '("tag4" "tag2/subtag2"))
           '(file1))
       (is (tagger::list-files col :-tags '("tag3") :+tags '("tag2"))
           '(file3))
       (is (tagger::list-files col :-tags '("tag4") :+tags '("tag2" "tag1"))
           '(file3))
       (is (tagger::list-files col :-tags '("tag4" "tag2/subtag2") :+tags '("tag1"))
           '(file1))
       (is (tagger::list-files col :-tags '("tag4" "tag2/subtag2") :+tags '("tag1" "tag2/subtag2"))
           '())
       (is (tagger::list-files col :-tags '("tag4") :+tags '("tag4"))
           '())
       )))
           
