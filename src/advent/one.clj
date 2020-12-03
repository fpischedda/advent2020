(ns advent.one
  (:require [advent.utils :refer [read-input-lines]]))

(defn read-data [filename]
  (mapv #(Integer/parseInt %) (read-input-lines filename)))

(defn find-sum
  "returns the first two elements in `values` whos sum is
   equal to `check-num`"
  [values check-num]
  (loop [val (first values)
         others (rest values)]
    (when val
      (if-let [other (reduce
                       (fn [_ v]
                         (when (= check-num (+ v val))
                           (reduced v)))
                       nil others)]
        [val other]
        (recur (first others) (rest others))))))

(defn find-sum-2
  "returns the first three elements in `values` whos sum is
   equal to `check-num`"
  [values check-num]
  (loop [val (first values)
         others (rest values)]
    (if-let [[val2 val3] (find-sum others (- check-num val))]
      (* val val2 val3)
      (recur (first others) (rest others)))))

(defn solution-1 []
  (reduce * (find-sum (read-data "resources/one.data") 2020)))

(defn solution-2 []
  (find-sum-2 (read-data "resources/one.data") 2020))

(comment
  ;; solution for day 1, problem 1
  (solution-1) ;; => 1016964
  (solution-2) ;; => 182588480
  )
