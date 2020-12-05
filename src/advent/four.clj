(ns advent.four
  (:require [clojure.set :as set]
            [clojure.java.io :as io]))

(def required-fields #{"byr"
                       "iyr"
                       "eyr"
                       "hgt"
                       "hcl"
                       "ecl"
                       "pid"})

(defn valid-passport-keys?
  [passport]
  (empty? (set/difference required-fields passport)))

(defn extract-passport-keys
  [line]
  (->> line
    (re-seq #"(\w+):")
    (map second)
    set))

(defn count-valid-passports-keys
  [batch-lines]
  (->> batch-lines
    (reduce
      (fn [{:keys [valid passport] :as acc} line]
        (if (empty? line)
          (assoc acc :passport #{}
            :valid (if (valid-passport-keys? passport) (inc valid) valid))
          (assoc acc :passport (set/union passport (extract-passport-keys line)))))
      {:valid 0 :passport #{}})
    :valid))

(defn solution1
  [filename]
  (with-open [rdr (io/reader filename)]
    (-> rdr
      line-seq
      count-valid-passports-keys)))

(comment
  (solution1 "resources/four.data")  ;; => 237
  )


(defn parse-passport-line
  [line]
  (->> line
    (re-seq #"(\w+):(#?\w+)")
    (mapv (fn [[_ k v]] [k v]))
    (into {})))

(defn between
  [min max]
  (fn [value]
    (<= min (Integer/parseInt value) max)))

(def cm-validator (between 150 193))
(def in-validator (between 59 76))

(defn height
  [value]
  (when-let [[_ num-str unit] (re-matches #"(\d+)(cm|in)" value)]
    (case unit
      "cm" (cm-validator num-str)
      "in" (in-validator num-str)
      :else nil)))

(defn color
  [value]
  (re-matches #"#[a-f0-9]{6}" value))

(defn nine-digits
  [value]
  (re-matches #"[0-9]{9}" value))

(def validators
  {"byr" (between 1920 2002)
   "iyr" (between 2010 2020)
   "eyr" (between 2020 2030)
   "hgt" height
   "hcl" color
   "ecl" #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"}
   "pid" nine-digits})

(defn valid-passport?
  [passport]
  (reduce-kv
    (fn [_ k validator-fn]
      (if-let [value (get passport k)]
        (if-not (validator-fn value)
          (reduced false)
          true)
        (reduced false))) ;; missing required key
    true validators))

(defn count-valid-passports
  [batch-lines]
  (->> batch-lines
    (reduce
      (fn [{:keys [valid passport] :as acc} line]
        (if (empty? line)
          (assoc acc
            :passport {}
            :valid (if (valid-passport? passport) (inc valid) valid))
          (assoc acc :passport (merge passport (parse-passport-line line)))))
      {:valid 0 :passport {}})
    :valid))

(def test-data ["ecl:hzl byr:1926 iyr:2010"
                "pid:221225902 cid:61 hgt:186cm eyr:2021 hcl:#7d3b0c"
                ""
                "hcl:#efcc98 hgt:178 pid:433543520"
                "eyr:2020 byr:1926"
                "ecl:blu cid:92"
                "iyr:2010"
                ""
                "hcl:#efcc98 hgt:178cm pid:433543520"
                "eyr:2020 byr:1926"
                "ecl:blu cid:92"
                "iyr:2010"
                ""
                ])

(defn solution2
  [filename]
  (with-open [rdr (io/reader filename)]
    (-> rdr
      line-seq
      count-valid-passports)))

(comment
  (solution2 "resources/four.data")  ;; => 172
  )
