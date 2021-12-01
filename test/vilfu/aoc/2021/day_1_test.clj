(ns vilfu.aoc.2021.day-1-test
  (:require [clojure.test :refer :all]
            [vilfu.aoc.2021.day-1 :as sut]))

(deftest scanning-the-floor
  (is (= [[1 :NA] [2 :increased] [2 :no-change]]
         (sut/scan-floor [1 2 2]))))

(deftest measurements-are-larger
  (is (= 1 ;; increase...
         (sut/larger-measurements (sut/scan-floor [1 2 2])))))

(deftest window-scanning
  (is (= [(+ 1 2 3) (+ 2 3 4) (+ 3 4 5) (+ 4 5 6)]
         (sut/scan-window [1 2 3 4 5 6]))))
