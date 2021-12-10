(ns vilfu.aoc.2021.day-09)

(def example-bits
  "00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010")

(comment

  (update-in [] [0] assoc \1 1)
  
  (let [{:keys [sums count]}
        (->> example-bits
             clojure.string/split-lines
             (map #(clojure.string/split % #"[01]{0}"))
             (map (fn [ss]
                    (map #(Integer/parseInt %) ss)))
             (reduce (fn [{:keys [sums] :as ctx} bits]
                       (-> ctx
                           (update :count (fnil inc 0))
                           (assoc :sums (mapv + sums bits))))
                     {:sums (repeat 0)}))
        gamma-array (map (fn [sum]
                           (if (< count (* sum 2))
                             1
                             0))
                         sums)
        epsilon-array (map #(- 1 %) gamma-array)
        gamma (reduce (fn [prev next]
                        (+ (* 2 prev) next))
                      gamma-array)
        epsilon (reduce (fn [prev next]
                        (+ (* 2 prev) next))
                      epsilon-array)]
    (* gamma epsilon))

  
  

  (clojure.string/split "110111" #"[01]{0}")
  
  )


(defn compute-power-consumption [input]
    (let [{:keys [sums count]}
        (->> input
             clojure.string/split-lines
             (map #(clojure.string/split % #"[01]{0}"))
             (map (fn [ss]
                    (map #(Integer/parseInt %) ss)))
             (reduce (fn [{:keys [sums] :as ctx} bits]
                       (-> ctx
                           (update :count (fnil inc 0))
                           (assoc :sums (mapv + sums bits))))
                     {:sums (repeat 0)}))
        gamma-array (map (fn [sum]
                           (if (< count (* sum 2))
                             1
                             0))
                         sums)
        epsilon-array (map #(- 1 %) gamma-array)
        gamma (reduce (fn [prev next]
                        (+ (* 2 prev) next))
                      gamma-array)
        epsilon (reduce (fn [prev next]
                        (+ (* 2 prev) next))
                      epsilon-array)]
    (* gamma epsilon)))

(comment
  (compute-power-consumption (slurp "resources/inputs/input_day_03.txt"))
  ;; => 2967914
  )
