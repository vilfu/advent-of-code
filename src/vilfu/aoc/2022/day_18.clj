(ns vilfu.aoc.2022.day-18
  (:require [clojure.string :as s])
  (:require [clojure.test :refer [is testing with-test]])
  (:require [vilfu.aoc.2022.handy :refer [example input]]))


(def example-input "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5")


(with-test
  (defn coords [lines]
    (reduce (fn [state input]
              (let [[x y z :as coord]
                    (map parse-long (s/split input #","))]
                (conj state (vec coord))))
            []
            lines))
  (is (= [[1 2 3]]
         (coords ["1,2,3"]))))

(comment

  (* 6 (count (example example-input)))
  
  (let [partitioned-x-wise (partition 3 1
                                      (sort-by first
                                               (group-by first
                                                         (coords (example example-input)))))]
    
    partitioned-x-wise)

  (let [coords (coords (example example-input))
        coord-set (set coords)
        connections (map
                     (fn [[x y z :as coord]]
                       (let [potential-neighbours  (set [[(dec x) y z]
                                                         [(inc x) y z]
                                                         [x (inc y) z]
                                                         [x (dec y) z]
                                                         [x y (inc z)]
                                                         [x y (dec z)]])
                             connected (clojure.set/intersection coord-set
                                                                 potential-neighbours)]
                         (conj coord :connected connected))) 
                     coords)
        sides (map
               (fn [[x y z _ connected]]
                 (- 6 (count connected)))
               connections)]
    {:sides (reduce + sides)
     :connections connections}
    )

  (let [x 1 y 2 z 3]
    (set [[(dec x) y z]
          [(inc x) y z]
          [x (inc y) z]
          [x (dec y) z]
          [x y (inc z)]
          [x y (dec z)]]))
  
  )

(defn neighbouring-coords [[x y z]]
  (set  [[(dec x) y z]
         [(inc x) y z]
         [x (inc y) z]
         [x (dec y) z]
         [x y (inc z)]
         [x y (dec z)]]))

(with-test
  (defn compute-non-connected-sides [lines]
    (let [coords (coords lines)
          coord-set (set coords)
          connections (map
                       (fn [[x y z :as coord]]
                        (let [connected
                              (clojure.set/intersection coord-set
                                                        (neighbouring-coords coord))]
                          (conj coord :connected connected))) 
                       coords)
          
          sides (map
                 (fn [[x y z _ connected]]
                   (- 6 (count connected)))
                connections)]
      (reduce + sides)))
  (is (= 64
         (compute-non-connected-sides (example example-input)))))


(comment
  (compute-non-connected-sides (input "resources/2022/inputs/day_18.txt"))
  )

(comment
  "Part 2"

  (let [coords (coords (example example-input))
        coord-set (set coords)
        cloud (reduce
               (fn [so-far [x y z :as coord]]
                 (let [potential-neighbours  (neighbouring-coords coord)
                       without-existing (clojure.set/difference potential-neighbours
                                                                coord-set)]
                   (clojure.set/union so-far
                                      without-existing)))
               #{}
               coords)]
    
    (->> cloud
         (map (fn [coord]
                (let [potential-neighbours (neighbouring-coords coord)
                      connected (clojure.set/intersection coord-set
                                                          potential-neighbours)]
                  (conj coord connected))))
         (filter (comp #(= 6 %) count last))
         ))
    )


(with-test
  (defn find-trapped-air [lines]
    (let [coords (coords lines)
          lava-set (set coords)
          touching-air (reduce
                        (fn [total-air [x y z :as coord]]
                          (let [potential-neighbours  (neighbouring-coords coord)
                                only-air (clojure.set/difference potential-neighbours
                                                                 lava-set)]
                            (clojure.set/union total-air
                                               only-air)))
                        #{}
                        coords)]
      
      (->> touching-air
           (map (fn [coord]
                  (let [potential-neighbours (neighbouring-coords coord)
                        connected (clojure.set/intersection lava-set
                                                            potential-neighbours)]
                    (conj coord connected))))
           (filter (comp #(= 6 %) count last))
           )))
  
  (is (= 1
         (->> (find-trapped-air (example example-input))
              count)))
  (is (= [2 2 5]
         (->> (find-trapped-air (example example-input))
              first
              (take 3)))))

(comment

  (clojure.set/difference #{1 2 3} #{2 3 4 5 6})
  
  (let [input-coords (input "resources/2022/inputs/day_18.txt")
        non-connected (compute-non-connected-sides input-coords)
        trapped-air  (find-trapped-air input-coords)
        trapped-air-count (count trapped-air)
        trapped-sides (* 6 trapped-air-count)
        calc `(- ~non-connected ~trapped-sides)]
    [:non-connected non-connected
     :trapped-air-count trapped-air-count
     :trapped-sides trapped-sides
     (pr-str calc) (eval calc)
     trapped-air])

  )
        
