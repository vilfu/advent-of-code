(ns vilfu.aoc.2022.day-21
  (:require [clojure.string :as s])
  (:require [clojure.test :refer [is testing with-test]])
  (:require [vilfu.aoc.2022.handy :refer [example input]]))


(def monkey-example "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32")

(defn parse-monkey
  ([label number] [:label label :number (parse-long number)])
  ([label _ arg1 op arg2] (let [operator (symbol op)]
                            [:label label :op operator [arg1 arg2]])))

(defn ->data [lines]
  (->> lines
   (map
    (fn [line]
      (->> line
           (re-matches #"([a-z]+): ([0-9]+|([a-z]+) ([+\-*/]) ([a-z]+))")
           rest
           (remove nil?)
           (apply parse-monkey)))
    )
   (reduce
    (fn [bag [_ _ type :as instruction]]
      (case type
        :number (let [[_ label _ num] instruction]
                  (assoc-in bag [:resolved label] num))
        :op (let [[_ label _ operator [arg1 arg2]] instruction]
              (assoc-in bag [:unresolved label] (list operator arg1 arg2)))))
    {})
   ))

(defn resolve-one-pass [state]
  (reduce
   (fn [{:keys [unresolved resolved to-resolve] :as state} node-name]
     (let [[op arg1 arg2] (get-in state [:unresolved node-name])]
       (cond
         (and (resolved arg1)
              (resolved arg2))
         (-> state
             (assoc-in [:resolved node-name]
                       (list op (resolved arg1) (resolved arg2)))
             (update :unresolved dissoc node-name)
             (update :to-resolve #(clojure.set/difference % #{node-name})))
         
         :otherwise
         (cond-> state
           (unresolved arg1) (update :to-resolve conj arg1)
           (unresolved arg2) (update :to-resolve conj arg2)))))
   state
   (state :to-resolve))
  )


(defn resolve-root-tree [parsed-state]
  (loop [state (assoc parsed-state :to-resolve #{"root"})
         attempt 1]
    (let [{:keys [unresolved resolved]} state
          ]
      (when (< 1000 attempt)
        (assoc state :failure (str "Failed after " attempt " attempts")))
      (if (resolved "root")
        (get-in state [:resolved "root"])
        (recur (resolve-one-pass state) (inc attempt))))))

(comment

  (resolve-root-tree (->data (example monkey-example)))

  (->  "resources/2022/inputs/day_21.txt"
       input
       ->data
       resolve-root-tree
       ;; eval
       )


  
  (->> (assoc (->data (example monkey-example)) :to-resolve #{"root"})
       resolve-one-pass 
       resolve-one-pass
       resolve-one-pass
       resolve-one-pass
       resolve-one-pass
       resolve-one-pass
       resolve-one-pass
       )
  
   (def res
     (loop [state (assoc raw :to-resolve #{"root"})
            attempt 1]
       (let [{:keys [unresolved resolved]} state
             ]
         (when (< 1000 attempt)
           (assoc state :failure (str "Failed after " attempt " attempts")))
         (if (resolved "root")
           (get-in state [:resolved "root"])
           (recur (attempt-resolve state) (inc attempt))))))

   (eval res)
  
   )
