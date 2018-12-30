(ns hello.core-test
  (:require
   [cljs.test :refer-macros [is deftest]]
   [hello.core]))

;; Write a bad test to demonstrate a failure.
(deftest test-whom
  (is (= (hello.core/whom) "you")))
