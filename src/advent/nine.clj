(ns advent.nine
  (:require [advent.utils :refer [read-input-lines]]
            [advent.one :refer [find-sum]]))

(def data (read-input-lines "resources/nine.data"))
(def data-test (read-input-lines "resources/nine-test.data"))

(defn find-missing-sum
  [window-size values]
  (reduce
    (fn [_ idx]
      (let [num (nth values idx)]
        (if-let [_ (find-sum
                     (->> values
                       (drop (- idx window-size))
                       (take window-size))
                     num)]
          nil
          (reduced num))))
    nil (range window-size (count data))))

(defn find-consecutive-sum
  [sums-to values]
  (let [result
        (loop [candidates values]
          (if-let [found-seq
                   (reduce
                     (fn [{:keys [acc the-seq] :as iter} v]
                       (let [new-acc (+ acc v)]
                         (cond
                           (= new-acc sums-to)
                           (if (> (count the-seq) 0)
                             (reduced (conj the-seq v))
                             (reduced nil))
                           (> new-acc sums-to)
                           (reduced nil)
                           :else (assoc iter :acc new-acc :the-seq (conj the-seq v)))))
                     {:acc 0 :the-seq []} candidates)]
            found-seq
            (recur (rest candidates))))]
    (+ (apply min result) (apply max result))))

(comment

  ;; solution 1
  (->> data
    (mapv #(Long/parseLong %))
    (find-missing-sum 25)) ;; => 1639024365

  ;; solution 2
  (->> data-test
    (mapv #(Long/parseLong %))
    (find-consecutive-sum 127)) ;; =>

  ;; solution 2
  (->> data
    (mapv #(Long/parseLong %))
    (find-consecutive-sum 1639024365)) ;; => 219202240
  )
