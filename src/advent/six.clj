(ns advent.six
  (:require
   [clojure.java.io :as io]
   [clojure.set :refer [union intersection]]))

(defn parse-groups
  [data]
  (reduce (fn [{:keys [yes group] :as acc} line]
            (if (empty? line)
              (assoc acc :yes (+ yes (count group)) :group #{})
              (assoc acc :group (union group (set line)))))
    {:yes 0 :group #{}} data))

(comment

  (with-open [rdr (io/reader "resources/six.data")]
    (->> rdr
      line-seq
      parse-groups))
  )

(defn parse-groups-2
  [data]
  (reduce (fn [{:keys [yes group] :as acc} line]
            (if (empty? line)
              (assoc acc
                :yes (+ yes (count (apply intersection group)))
                :group [])
              (assoc acc :group (conj group (set line)))))
    {:yes 0 :group []} data))

(comment

  (with-open [rdr (io/reader "resources/six.data")]
    (->> rdr
      line-seq
      parse-groups-2))
  )
