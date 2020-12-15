(ns advent.ten
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :refer [expt]]))

(defn parse-data
  [values]
  (->> values
    (map #(Integer/parseInt %))
    sort
    vec))


(def data
  (with-open [rdr (io/reader "resources/ten.data")]
    (->> rdr
      line-seq
      parse-data)))

(def data-test
  (with-open [rdr (io/reader "resources/ten-test.data")]
    (->> rdr
      line-seq
      parse-data)))

(def data-test-tiny
  (with-open [rdr (io/reader "resources/ten-test-tiny.data")]
    (->> rdr
      line-seq
      parse-data)))

(defn get-differences
  [values]
  (reduce
    (fn [{:keys [prev diffs] :as acc} v]
      (assoc acc :prev v :diffs (conj diffs (- v prev))))
    {:prev 0 :diffs []}
    values))

(defn mul-jolt-diffs
  [values]
  (->> values
    get-differences
    :diffs
    frequencies
    (map (fn [[_ v]] v))
    (reduce *)))

(comment

  ;; solution 1
  (mul-jolt-diffs data) ;; => 2450
  )

;; this does not work as expected :(
(defn combinations[values]
  (let [differences
        (->> values
          get-differences
          :diffs
          )]
    (loop [[ones others] (split-with #(= 1 %) differences)
           total 0]
      (if (and (empty? ones) (empty? others))
        total
        (let [combs (count ones)
              new-total (if (> combs 1)
                          (+ combs (* total combs))
                          total)
              [_ new-ones] (split-with #(not= 1 %) others)]
          (recur (split-with #(= 1 %) new-ones) new-total))))))

;; shamelessly copied from here :D
;; https://www.youtube.com/watch?v=1CPuvWvJMFo
(def comb-count
  (memoize
    (fn
      ([_x] 1)
      ([ x y & zs]
       (if (> (+ x y) 3)
         (apply comb-count y zs)
         (+
           (apply comb-count y zs)
           (apply comb-count (+ x y) zs)))))))

(comment

  ;; solution 2
  (->> data
    get-differences
    :diffs
    (apply comb-count)
    ) ;; => 32396521357312

  ;; solution 2
  (->> data-test-tiny
    get-differences
    :diffs
    combinations
    ) ;;
  )
