(defpackage :tagger-test
   (:use :cl :fiveam))

(in-package :tagger-test)

(defvar *tfile* #p"/tmp/test-write-bit")

(def-suite :tagger-bits)
(in-suite :tagger-bits)

(test write-zero-to-empty-file
   "Write zeroes to an empty file"
    (tagger::write-bit 0 *tfile* 0)
    (is-false (probe-file *tfile*))
    (tagger::write-bit 0 *tfile* 10)
    (is-false (probe-file *tfile*)))

(test write-one-to-
     (tagger::write-bit 1 *tfile* 1)
     (is-true (probe-file *tfile*)))
