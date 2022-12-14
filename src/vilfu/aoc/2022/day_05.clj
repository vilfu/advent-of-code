(ns vilfu.aoc.2022.day-5
  (:require [clojure.string :as s])
  (:require [clojure.test :refer [is testing with-test]])
  (:require [vilfu.aoc.2022.handy :refer [example input]]))

(def example-state "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn crate-or-nil [s]
  (re-matches #"[A-Z]|[0-9]" s))

(comment
  (def start-state
    (let [start-section (->> example-state
                             example
                             (take-while (comp not empty?)))
          
          [rows [labels]] (split-with (comp not (fnil parse-long "") first)
                                      (mapv (fn [line]
                                              (mapv (comp crate-or-nil
                                                          str
                                                          second)
                                                    (partition 3 4 line)))
                                            start-section))]
      (zipmap
       (map parse-long labels)
       (map #(remove nil? %)
            (reduce (fn [outer row]
                      (reduce (fn [inner [tower crate]]
                                (update-in inner [tower] conj crate))
                              outer
                              (map-indexed vector row)))
                    []
                    (reverse rows))))))

  (let [instructions (->> example-state
                          example
                          (drop-while (comp not #(s/starts-with? % "move"))))]
    (reduce (fn [state instruction]
              (let [[n from to] (map parse-long
                                     (rest (re-matches #"move ([0-9]) from ([0-9]) to ([0-9])"
                                                       instruction)))]
                (reduce (fn [state _]
                          (let [crate (-> state (get from) first)]
                            (-> state
                                (update to conj crate)
                                (update from rest))))
                        state
                        (range n))))
            start-state
            instructions))

  (input "resources/2022/inputs/day_05.txt")
  
  )

(defn starting-state [lines]
  (let [start-section (take-while (comp not empty?) lines)
        [rows [labels]] (split-with (comp not (fnil parse-long "") first)
                                    (mapv (fn [line]
                                            (mapv (comp crate-or-nil
                                                        str
                                                        second)
                                                  (partition 3 4 line)))
                                          start-section))]
    (zipmap
     (map parse-long labels)
     (map #(remove nil? %)
          (reduce (fn [outer row]
                    (reduce (fn [inner [tower crate]]
                              (update-in inner [tower] conj crate))
                            outer
                            (map-indexed vector row)))
                  []
                  (reverse rows))))))

(defn instructions [lines]
  (drop-while (comp not #(s/starts-with? % "move")) lines))


(defn follow-instructions [start instructions]
  (reduce (fn [state instruction]
            (let [[n from to] (map parse-long
                                   (rest (re-matches #"move ([0-9]+) from ([0-9]) to ([0-9])"
                                                     instruction)))]
              (reduce (fn [state _]
                        (let [crate (-> state (get from) first)]
                          (-> state
                              (update to conj crate)
                              (update from rest))))
                      state
                      (range n))))
          start
          instructions))

(defn topmost-crates [state]
  (->> state
       (map identity)
       sort
       (map (comp first second))
       (apply str)))

(comment

  (def input-1   (input "resources/2022/inputs/day_05.txt"))

  (->> (follow-instructions (starting-state input-1)
                            (instructions input-1))
       topmost-crates)

  (take 2 '(1 2 3))
  (concat '(1 2) '(2 3))

  ()
  
  )

(defn follow-instructions-9001 [start instructions]
  (reduce (fn [state instruction]
            (let [[n from to] (map parse-long
                                   (rest (re-matches #"move ([0-9]+) from ([0-9]) to ([0-9])"
                                                     instruction)))]
              (let [crates (take n (get state from))]
                 (-> state
                     (update to #(concat crates %))
                     (update from #(drop n %))))))
          start
          instructions))


(comment

  (def input-1   (input "resources/2022/inputs/day_05.txt"))

  (->> (follow-instructions-9001 (starting-state input-1)
                                 (instructions input-1))
       topmost-crates)

  )
