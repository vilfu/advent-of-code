(ns vilfu.aoc.2021.day-09
  (:require [clojure.string :as s]))

(def example-data
  "2199943210
3987894921
9856789892
8767896789
9899965678")

(def char->int (comp #(- % 48) int))

(defn ->rows [input-str]
  (->> input-str
       s/split-lines
       (mapv #(mapv char->int %))))

(defn get-height-at [data row col]
    (get-in data [row col]))

(defn neighbours [data-rows]
  (for [[row-num row] (map vector (range) data-rows)
        [col-num val] (map vector (range) row)]
    [{:val val :row row-num :col col-num}
     (cond-> []
       (< 0 row-num)
       (conj (get-height-at data-rows (dec row-num) col-num))

       (< (inc row-num) (count data-rows))
       (conj (get-height-at data-rows (inc row-num) col-num))

       (< 0 col-num)
       (conj (get-height-at data-rows row-num (dec col-num)))

       (< (inc col-num) (count row))
       (conj (get-height-at data-rows row-num (inc col-num))))]))

(defn find-low-points [data-rows]
  (->> data-rows
       neighbours
       (filter  (fn [[{val :val} neighbours]]
                  (every? #(< val %) neighbours)))
       (map first)))

(defn sum-risk-levels [input-str]
  (->> input-str
       ->rows
       find-low-points
       (map :val)
       (map inc)
       (reduce +)))


(defn find-neighbours-coords [data {:keys [row col]}]
    (cond-> []
      (< 0 row)
      (conj [(dec row) col])

      (< (inc row) (count data))
      (conj [(inc row) col])

      (< 0 col)
      (conj [row (dec col)])

      (< (inc col) (count (get data row)))
      (conj [row (inc col)])))

(defn expand-basin-around [data point]
  (loop [basin #{point}]
    (let [expansion (set (mapcat
                          (fn [point]
                            (->> point
                                 (find-neighbour-coords data)
                                 (map (fn [[row col]]
                                        {:row row
                                         :col col
                                         :val (get-height-at data row col)}))
                                 (remove (comp (partial = 9) :val))
                                 (remove basin)))
                          basin))]
      (if (empty? expansion)
        basin
        (recur (clojure.set/union basin expansion))))))

(defn find-basins [data]
  (for [point (find-low-points data)]
    (expand-basin-around data point)))

(defn basin-sizes-multiplied [input]
  (->> input
      ->rows
      find-basins
      (map count)
      (sort >)
      (take 3)
      (reduce *)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Experimental area
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(comment
  (def char->int (comp #(- % 48) int))

  (def index (->> example-data
                  s/split-lines
                  (mapv #(mapv char->int %))))

  index
  
  (defn get-height-at [row col]
    (get-in index [row col]))
  
  (get-height-at 0 0)

  (->> (for [[row-num row] (map vector (range) index)
             [col-num val] (map vector (range) row)]
         [val (cond-> []
                (< 0 row-num)
                (conj (get-height-at (dec row-num) col-num))

                (< (inc row-num) (count index))
                (conj (get-height-at (inc row-num) col-num))

                (< 0 col-num)
                (conj (get-height-at row-num (dec col-num)))

                (< (inc col-num) (count row))
                (conj (get-height-at row-num (inc col-num))))])
       (filter  (fn [[val neighbours]]
                  (every? #(< val %) neighbours)))
       (map first))

  (get-height-at 123 123)

  ((comp #(- % 48) int) \2))

(comment
  (sum-risk-levels example-data)
  ;; => 15

  (def input-1 (slurp "resources/inputs/input_day_09.txt"))

  (->> input-1
       ->rows
       neighbours
       )

  (sum-risk-levels input-1)
  ;; => 580

  

  (def example-rows (->> example-data
                         ->rows))

  (->>  example-rows
        find-low-points
        (map #(find-neighbour-coords example-rows %)))  
    
  (let [data (->> example-data
                  ->rows)
        low-points (find-low-points data)]
    (for [point low-points]
      (expand-basin-around data point)))

  )

(comment
  (basin-sizes-multiplied example-data)
  ;; => 1134

  (basin-sizes-multiplied (slurp "resources/inputs/input_day_09.txt"))
  ;; => 856716

  )
