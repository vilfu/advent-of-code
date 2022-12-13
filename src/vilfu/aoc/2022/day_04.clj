(ns vilfu.aoc.2022.day-4
  (:require [clojure.string :as s])
  (:require [clojure.test :refer [is testing with-test]])
  (:require [clojure.set :refer [superset? subset? intersection]]))


(def example-ranges "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn parse-int [s]
  (assert (string? s))
  (Integer/parseInt s))

(defn input [path]
  (-> path
      clojure.java.io/reader
      line-seq))

(defn example []
  (-> example-ranges
      (java.io.StringReader.)
      clojure.java.io/reader
      line-seq))

(comment
  "Experimenting..."

  (->> (example)
      (map #(re-matches #"([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)" %))
      (map rest)
      (map #(mapv parse-int %))
      (map (fn [[a b c d]]
             (map set [(range a (inc b)) (range c (inc d))])))
      (map (fn [[e1 e2]]
             (or (subset? e1 e2)
                 (subset? e2 e1))))
      (filter true?)
      count
   )
  
  
  )

(defn complete-overlap? [[set1 set2]]
  (or (subset? set1 set2)
      (subset? set2 set1)))

(with-test
  (defn count-overlaps [lines overlap-pred]
    (->> lines
         (map #(re-matches #"([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)" %))
         (map rest)
         (map #(mapv parse-int %))
         (map (fn [[a b c d]]
                (map set [(range a (inc b)) (range c (inc d))])))
         (filter overlap-pred)
         count))

  (is (= (count-overlaps (example)
                         complete-overlap?)
         2)))

(comment
  "For realz, first solution"

  (count-overlaps (input "resources/2022/inputs/day_04.txt")
                  complete-overlap?)
  )

(defn any-overlap? [[set1 set2]]
  (not (empty? (intersection set1 set2))))

(comment
  "Second solution"  

  (count-overlaps (input "resources/2022/inputs/day_04.txt")
                  any-overlap?))
