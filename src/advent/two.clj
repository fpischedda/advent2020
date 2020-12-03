(ns advent.two
  (:require [advent.utils :refer [read-input-lines]]))

(def line-regex #"^(\d+)-(\d+) (\w): (\w+)$")

(comment
  (re-find line-regex "15-19 k: kkkkkkkkkkkkzkkkkkkk")
  ;; => ["15-19 k: kkkkkkkkkkkkzkkkkkkk" "15" "19" "k" "kkkkkkkkkkkkzkkkkkkk"]

  )

(defn parse-line
  "a line of input is composed by:
  - a range in the form of min-max
  - a blank space
  - a char which must be present at least min and at most max times in the password
  - a column character
  - a blank space
  - and finally the password

  the function will return a map with the following shape:
  {:min 1
   :max 3
   :character a
   :password abcd}"
  [line]
  (let [[_ min max char password] (re-find line-regex line)]
    {:min (Integer/parseInt min)
     :max (Integer/parseInt max)
     :char char
     :password password}))

(comment
  (parse-line "15-19 k: kkkkkkkkkkkkzkkkkkkk")
  ;; => {:min 15, :max 19, :char "k", :password "kkkkkkkkkkkkzkkkkkkk"}

  )

(defn read-data [filename]
  (mapv parse-line (read-input-lines filename)))

(defn valid-password?
  [{:keys [min max char password]}]
  (let [n (count (re-seq (re-pattern char) password))]
    (and (>= n min) (<= n max))))

(comment
  (->  "15-19 k: kkkkkkkkkkkkzkkkkkkk"
    parse-line
    valid-password?) ;; => true

  (->  "1-3 k: aaaa"
    parse-line
    valid-password?) ;; => false
  )

(defn valid-password-fix?
  [{:keys [min max char password]}]
  (let [c1 (nth password (dec min))
        c2 (nth password (dec max))
        compare (nth char 0)]
    (cond
      (and (= c1 compare) (= c2 compare)) false
      (= c1 compare) true
      (= c2 compare) true
      :else false)))

(comment

  (valid-password-fix? {:min 1 :max 3 :char "a" :password "abc"}) ;; => true
  (valid-password-fix? {:min 1 :max 3 :char "a" :password "aba"}) ;; => false
  (valid-password-fix? {:min 1 :max 3 :char "k" :password "aba"}) ;; => false
  )


(comment
  ;; solution 1
  (->> "resources/two.data"
    read-data
    (filterv valid-password?)
    count) ;; => 655

  ;; solution 2
  (->> "resources/two.data"
    read-data
    (filterv valid-password-fix?)
    count) ;; => 673
)
