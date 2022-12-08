(ns vilfu.aoc.2022.day-3
  (:require [clojure.string :as s])
  (:require [clojure.test :refer [is testing with-test]]))



(def example-contents "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw")

(def priorities
  (zipmap
   (map char (concat (range (int \a) (inc (int \z)))
                     (range (int \A) (inc (int \Z)))))
   (map inc (range))))

(comment "Experimental area for first solution"
  (->> (java.io.StringReader. example-contents)
      clojure.java.io/reader
      line-seq
      (map (fn [s]
             (let [len (.length s)
                   _ (assert (even? len))
                   half (/ len 2)]
               (mapv set (split-at half s)))))
      (map #(apply clojure.set/intersection %))
      (map first)
      (map priorities)
      (reduce +))

  )

(with-test
 (defn sum-priorities [lines]
   (->> lines
        (map (fn [s]
               (let [len (.length s)
                     _ (assert (even? len))
                     half (/ len 2)]
                 (mapv set (split-at half s)))))
        (map #(apply clojure.set/intersection %))
        (map first)
        (map priorities)
        (reduce +)))

  (is (= (sum-priorities (->> (java.io.StringReader. example-contents)
                              clojure.java.io/reader
                              line-seq))
         157)))

(defn input [path]
  (-> path
      clojure.java.io/reader
      line-seq))

(comment "Find first answer"
  (def answer-one
    (->> "resources/2022/inputs/day_03.txt"
         input
         sum-priorities))

  )


(comment
  "Experimental area for second part"
  
  (->> "resources/2022/inputs/day_03.txt"
       input
       (partition 3)
       (map (fn [ss]
              (->> ss
                   (map set)
                   (apply clojure.set/intersection)
                   first
                   priorities)))
       (reduce +))

  )
