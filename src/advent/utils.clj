(ns advent.utils
  (:require [clojure.string :refer [split-lines]]))

(defn read-input-lines [filename]
  (-> filename
      slurp
      split-lines))
