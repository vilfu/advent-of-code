(ns vilfu.aoc.2022.day-2
  (:require [clojure.string :as s])
  (:require [clojure.test :refer [is testing with-test]])
  (:require [clojure.set :refer [map-invert]]))

(def opponents
  {
   :A :Rock
   :B :Paper
   :C :Scissors
   })

(def mine {
           :X :Rock
           :Y :Paper
           :Z :Scissors
           })

(def scores
  {:Rock 1
   :Paper 2
   :Scissors 3

   :loss 0
   :draw 3
   :win 6})

(def beats
  {:Rock :Scissors
   :Paper :Rock
   :Scissors :Paper})

(defn wins [[opp me]]
  (cond
    (= (beats opp) me) [:loss me]
    (= (beats me) opp) [:win me]
    (= opp me) [:draw me]))

(def example-strategy "A Y
B X
C Z")

(comment

  (wins :Scissors :Rock)
  
  (->> example-strategy
       s/split-lines
       (map #(s/split % #" "))
       (map #(mapv keyword %))
       (map (juxt (comp opponents first) (comp mine second)))
       (map wins)
       flatten
       (map scores)
       (reduce +))
  )

(with-test
  (defn neat-pairs [lines]
    (map (comp (juxt (comp opponents first) (comp mine second))
               #(mapv keyword %)
               #(s/split % #" "))
         lines))

  (is (= (neat-pairs ["A Y"])
         [[:Rock :Paper]])))

(defn score [pairs]
  (->> pairs
       (map wins)
       flatten
       (map scores)
       (reduce +)))

(comment
  (-> example-strategy
      s/split-lines
      neat-pairs
      score)

  (def answer-one
    (-> (slurp "resources/2022/inputs/day_02.txt")
      s/split-lines
      neat-pairs
      score)
    )
  )

(def strategy
  {:X :loss
   :Y :draw
   :Z :win})

(with-test
  (defn neat-pairs-2 [lines]
    (map (comp (juxt (comp opponents first) (comp strategy second))
               #(mapv keyword %)
               #(s/split % #" "))
         lines))

  (is (= (neat-pairs-2 ["A Y"])
         [[:Rock :draw]])))

(def beaten-by (map-invert beats))

(defn choose-hand [[opp strat]]
  (case strat
    :loss (beats opp)
    :win (beaten-by opp)
    :draw opp))

(comment
  (->> example-strategy
      s/split-lines
      neat-pairs-2
      (map (juxt first choose-hand))
      score)

  (def answer-two
    (->> (slurp "resources/2022/inputs/day_02.txt")
      s/split-lines
      neat-pairs-2
      (map (juxt first choose-hand))
      score))
  
  )
