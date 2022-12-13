(ns vilfu.aoc.2022.handy
  (:require [clojure.string :as s])
  (:require [clojure.test :refer [is testing with-test]]))

(defn parse-int [s]
  (assert (string? s))
  (Integer/parseInt s))

(defn input [path]
  (-> path
      clojure.java.io/reader
      line-seq))

(defn example [example-string]
  (-> example-string
      (java.io.StringReader.)
      clojure.java.io/reader
      line-seq))
