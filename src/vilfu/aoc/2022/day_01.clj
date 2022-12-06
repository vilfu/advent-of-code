(ns vilfu.aoc.2022.day-1
  (:require [clojure.string :as s])
  (:require [clojure.test :refer [is testing with-test]]))

(def test-sample "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000")

(comment

  ;; First attempt, complicated reduce function
  (last
   (sort
    (:elves
     (reduce
      (fn [agg val]
        (if (= "" val)
          (-> agg
              (update-in [:elves] conj (agg :current))
              (assoc :current 0))
          (update-in agg [:current] (fnil + 0) (Integer/parseInt val))))
      {:current 0
       :elves []}
      (s/split-lines test-sample)))))

  ;; Second attempt, mapping, grouping, partitioning
  (first
   (sort >
         (map
          #(apply + %)
          (map
           (fn [x] (map #(Integer/parseInt %) x))
           (filter
            (comp not #(every? empty? %))
            (partition-by empty? (s/split-lines test-sample)))))))
  
  )

(defn parse-int [s]
  (Integer/parseInt s))

(with-test
  (defn most-calories [lines n]
    (take n
     (sort >
           (map
            #(reduce + %)
            (map
             (fn [x] (map parse-int x))
             (filter
              (comp not #(every? empty? %))
              (partition-by empty? lines)))))))

  (is (= [1000] (most-calories ["1000" ""] 1)))

  (testing "Don't forget that last, trailing group without a newline"
    (is (= [3000] (most-calories ["1000"
                                  "1000"
                                  ""
                                  "3000"] 1))))

  (testing "Test input"
    (is (= [24000]
           (most-calories (s/split-lines test-sample) 1)))))

(comment
  (def answer-to-first
    (-> "resources/2022/inputs/day_01.txt"
        slurp
        s/split-lines
        (most-calories 1)
        first))

  (def answer-to-second
    (reduce +
            (-> "resources/2022/inputs/day_01.txt"
                slurp
                s/split-lines
                (most-calories 3)
                )))
  )
