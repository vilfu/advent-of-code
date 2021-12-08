(ns vilfu.aoc.2021.day-2
  (:require [clojure.string :as s]))

(comment

  (->> (slurp "resources/inputs/input_day_2_test.txt")
       clojure.string/split-lines
       (map #(clojure.string/split % #" "))
       (map (fn [[mov val]] [mov (Integer/parseInt val)]))
       (map (fn [[mov val]]
              (let [dir (if (= mov "forward")
                          "horiz"
                          "depth")
                    change (if (= mov "up")
                             (- val)
                             val)]
                [dir change])))
       (reduce (fn [agg [pos val]]
                 (update agg pos + val))
               {"horiz" 0 "depth" 0}))
       
  )

(defn compute [input-str]
  (let [{:keys [horiz depth]} (->> input-str
                                   clojure.string/split-lines
                                   (map #(clojure.string/split % #" "))
                                   (map (fn [[mov val]] [mov (Integer/parseInt val)]))
                                   (map (fn [[mov val]]
                                          (let [dir (if (= mov "forward")
                                                      :horiz
                                                      :depth)
                                                change (if (= mov "up")
                                                         (- val)
                                                         val)]
                                            [dir change])))
                                   (reduce (fn [agg [pos val]]
                                             (update agg pos + val))
                                           {:horiz 0 :depth 0}))]
    (* horiz depth)))


(comment
  (compute (slurp "resources/inputs/input_day_2_test.txt"))
  (compute (slurp "resources/inputs/input_day_2.txt"))
;; => 1693300
  )

(defn next-state [{:keys [aim horiz depth] :as state} [instruction val]]
  (case instruction
    :up (update state :aim - val)
    :down (update state :aim + val)
    :forward (-> state
               (update :horiz + val)
               (update :depth + (* val aim)))))

(defn compute-using-aim [instructions]
  (let [{:keys [horiz depth]} (reduce next-state
                                      {:horiz 0 :depth 0 :aim 0}
                                      instructions)]
    (* horiz depth)))

(defn slurp-instructions [filename]
  (->> (slurp filename)
         s/split-lines
         (map #(s/split % #"\s"))
         (map (juxt (comp keyword first) (comp #(Integer/parseInt %) second)))))

(comment
  (def test-instructions (slurp-instructions "resources/inputs/input_day_2_test.txt"))
  (def assignment-instructions (slurp-instructions "resources/inputs/input_day_2.txt"))

  
  (compute-using-aim test-instructions)
  (compute-using-aim [[:up 1]])

  (next-state {:aim 0 :depth 0 :horiz 0} [:up 1])

  (-> "123"
      first
      str)

  (compute-using-aim assignment-instructions)
;; => 1857958050  
  )
