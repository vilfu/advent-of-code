(ns vilfu.aoc.2021.day-09-test
  (:require [clojure.test :refer :all]
            [vilfu.aoc.2021.day-09 :as sut]))

(deftest make-rows
  (is (= [[1 2 3]
          [4 5 6]]
         (sut/->rows "123\n456"))))

(def test-data [[1 2 3]
                [4 5 6]
                [7 8 9]])

(deftest height-map
  (testing "does-not-throw"
    (is (= nil
           (sut/get-height-at test-data -1 -1))
        "lower bounds")
    (is (= nil
           (sut/get-height-at test-data 123 123))
        "upper bounds")))

(deftest neighbours-first-iteration
  (testing "form"
    (is (= [{:val 1, :row 0, :col 0} [4 2]]
           (first (sut/neighbours test-data)))))
  (testing "has neighbours"
    (is (= [1 2]
           (map (comp :val first)
                (sut/neighbours [[1] [2]])))))
  (testing "neighbours are"
    (is (= (set [[1] [2]])
           (set (map second
                 (sut/neighbours [[1] [2]])))))))

(deftest low-point
  (is (= (sut/find-low-points test-data)
         '({:val 1, :row 0, :col 0})
         )))

(deftest neighbours-second-iteration
  (is (= (sut/find-neighbours-coords test-data {:row 0 :col 0})
         [[1 0] [0 1]]))
  (is (= (set (sut/find-neighbours-coords test-data {:row 1 :col 1}))
         (set
          [      [1 0]
           [0 1] ,,,,, [2 1]
                 [1 2]]))))

(deftest expand-basin
  (is (true? (->> {:row 0 :col 0}
                (sut/expand-basin-around test-data)
                (map :val) 
                (not-any? #(= 9 %))
                )
       )))
