(defpackage layoup/tests/main
  (:use :cl
        :layoup
        :rove))
(in-package :layoup/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :layoup)' in your Lisp.

(deftest test-target-1
    (testing "should (= 1 1) to be true"
	     (ok (= 1 1))))

(deftest test-target-2
    (testing "function keys outputs correct matrix"
	     (ok ((equal
		   (layoup:key-matrix
		    (list "qwertyuiop"
			  "asdfghjkl;"
			  "zxcvbnm,./"))
		   '((#\q #\w #\e #\r #\t #\y #\u #\i #\o #\p)
		     (#\a #\s #\d #\f #\g #\h #\j #\k #\l #\;)
		     (#\z #\x #\c #\v #\b #\n #\m #\, #\. #\/))))
