(ns advent.three
  (:require [advent.utils :refer [read-input-lines]]))


(def input (read-input-lines "resources/three.data"))
(def width (count (first input)))

(defn count-trees [input width h-slope v-slope]
  (reduce (fn [{:keys [trees x y] :as acc} line]
            (let [new-y (inc y)]
              (if (= new-y v-slope)
                (assoc acc
                  :x (mod (+ x h-slope) width)
                  :y 0
                  :trees (if (= \# (nth line x))
                           (inc trees)
                           trees))
                (assoc acc :y new-y))))
      {:trees 0 :x h-slope :y 0} (rest input)))

(defn count-trees-2 [input width h-slope v-slope]
  (let [rows (count input)]
    (loop [y v-slope
           x h-slope
           trees 0]
      (if (< y rows)
        (recur
          (+ y v-slope)
          (+ x h-slope)
          (if (= \# (-> input (nth y) (nth (mod x width))))
            (inc trees)
            trees))
        trees))))

(comment

  ;; solution 1
  (count-trees input width 3 1)
  ;; alternative 1
  (count-trees-2 input width 3 1)

  ;; solution 2
  (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
    (map #(count-trees input width (nth % 0) (nth % 1)))
    (map :trees)
    (reduce *)) ;; => 958815792
  ;; alternative 2
  (->> [[1 1] [3 1] [5 1] [7 1] [1 2]]
    (map #(count-trees-2 input width (nth % 0) (nth % 1)))
    (reduce *)) ;; => 958815792
  )
