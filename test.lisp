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
            (ungulp (str "/tmp/tagger-test-col/file" i ".txt") "file contents"))
          (let ((,col (tagger::make-col :name (str (gensym)) :custom-extensions (list ".txt"))))
            ,@body))
       (rm "/tmp/tagger-test-col" :recursive t)))


(deftest basic-tag-untag ()
  (with-test-col (col)
    (update-master-index col)
    (awith "/tmp/tagger-test-col/file1"
        (tagger::tag col it "tag1")
        (is (tagger::read-bit ) 1))
    (tagger::tag col "/tmp/tagger-test-col/file100" "tag1")
