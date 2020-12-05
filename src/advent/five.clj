(ns advent.five
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(defn decode-boarding-pass
  [boarding-pass]
  (let [{:keys [row-low col-low]}
        (reduce (fn [{:keys [row-low row-high col-low col-high] :as acc} ch]
                  (case ch
                    \F (assoc acc :row-high (int (/ (+ row-low row-high)   2)))
                    \B (assoc acc :row-low  (int (/ (+ row-low row-high 1) 2)))
                    \L (assoc acc :col-high (int (/ (+ col-low col-high)   2)))
                    \R (assoc acc :col-low  (int (/ (+ col-low col-high 1) 2)))))
          {:row-low 0 :row-high 127
           :col-low 0 :col-high 7} boarding-pass)]
    {:row row-low
     :col col-low}))

(comment
  (decode-boarding-pass "BBFFBBFRLL")

  ;; solution 1
  (with-open [rdr (io/reader "resources/five.data")]
    (->> rdr
      line-seq
      (map decode-boarding-pass)
      (map (fn [{:keys [row col]}] (+ col (* row 8))))
      (reduce (fn [max v] (if (> v max) v max))))) ;; => 976
      ;; ^^^ shorter version (apply max)

  ;; solution 2
  (let [seats (set (for [r (range 0 128)
                         c (range 0 8)]
                     (+ c (* r 8))))
        ]
    (with-open [rdr (io/reader "resources/five.data")]
      (->> rdr
        line-seq
        (map decode-boarding-pass)
        (map (fn [{:keys [row col]}] (+ col (* row 8))))
        set
        (set/difference seats)
        (sort))) ;; out of the possible solutions 685 is the right one
    )
  )
