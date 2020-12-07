(ns advent.seven
  (:require [clojure.string :refer [split]]
            [clojure.java.io :as io]))

(defn parse-line
  [line]
  (let [[bag contain] (split line #" bags contain ")]
    [bag
     (if (= contain "no other bags.")
       #{}
       (->> (split contain #", ")
         (map (fn [c] (let [[_ how-many bag]
                            (re-matches #"(\d) (\w+ \w+) .*" c)]
                        {:how-many (Integer/parseInt how-many)
                         :bag bag})))
         set))]))

(comment
  (re-matches #"(\d) (\w+ \w+) .*" "2 muted yellow bags.")
  (re-matches #"(\d) (\w+ \w+) .*" "1 bright white bag")
  (parse-line "light red bags contain 1 bright white bag, 2 muted yellow bags.")
  )

(defn map-contained-by
  [bags]
  (reduce (fn [containers [bag contained]]
            (reduce (fn [acc c]
                      (let [contained-bag (:bag c)
                            entry (get acc contained-bag #{})]
                        (assoc acc contained-bag (conj entry bag))))
               containers contained))
    {} bags))

(defn find-containers
  [bag-kind bag-containers]
  (if-let [contained-by (get bag-containers bag-kind)]
    (apply clojure.set/union contained-by
      (map #(find-containers % bag-containers) contained-by))
    #{}))

(comment

  ;; solution 1
  (with-open [rdr (io/reader "resources/seven.data")]
    (->> rdr
      line-seq
      (map parse-line)
      map-contained-by
      (find-containers "shiny gold")
      count))
  )

(defn count-bags
  [starting-bag bags-spec]
  (reduce (fn [acc {:keys [how-many bag]}]
            (+ acc how-many (* how-many (count-bags bag bags-spec)) ))
    0 (get bags-spec starting-bag)))

(comment

  ;; solution 2
  (with-open [rdr (io/reader "resources/seven.data")]
    (->> rdr
      line-seq
      (map parse-line)
      (into {})
      (count-bags "shiny gold"))) ;; => 54803

  )
