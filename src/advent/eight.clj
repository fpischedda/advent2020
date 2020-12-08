(ns advent.eight
  (:require [clojure.java.io :as io]))

(defn parse-instruction
  [line]
  (let [[_ op sign value]
        (re-matches #"(\w{3}) (\+|-)(\d+)" line)]
    {:op (keyword op)
     :value (* (Integer/parseInt value) (if (= sign "-" ) -1 1))}))

(defmulti execute (fn [instruction _state] (:op instruction)))

(defmethod execute :nop [_ state]
  (assoc state :ip (inc (:ip state))))

(defmethod execute :acc [{:keys [value]} {:keys [ip acc] :as state}]
  (assoc state :ip (inc ip) :acc (+ acc value)))

(defmethod execute :jmp [{:keys [value]} {:keys [ip] :as state}]
  (assoc state :ip (+ ip value)))

(defn detect-loop
  [instructions]
  (loop [state {:acc 0 :ip 0}
         ips-seen #{}]
    (let [curr-ip (:ip state)
          {:keys [ip acc] :as new-state}
          (execute (nth instructions curr-ip) state)]
      (if (contains? ips-seen ip)
        acc
        (recur new-state (conj ips-seen ip))))))

(defn fix-loop
  "Problem:
   It looks like that there is a faulty :jmp or :nop instructution
   in the 'boot' code which causes an infinite loop.
   Solution:
   To find it, this function will try to execute a modified version
   of the program, changing a :jmp to :nop or :nop to :jmp.
   As soon as the program starts looping, the state will be reverted
   at the point of the change and will try with the next :jmp or :nop
   operation until the program will reach the last instruction."
  [instructions]
  (let [last-ip (count instructions)]
    (loop [state {:acc 0 :ip 0}
           ips-seen #{}
           branch-state nil
           ignore false]
      (if (>= (:ip state) last-ip)
        (:acc state)
        (let [instr (nth instructions (:ip state))
              op (:op instr)
              [new-branch instr-fix]
              (cond
                (or ignore (some? branch-state)) [branch-state instr]
                (= op :jmp) [{:state state :seen ips-seen}
                             (assoc instr :op :nop)]
                (= op :nop) [{:state state :seen ips-seen}
                             (assoc instr :op :jmp)]
                :else [branch-state instr])
              {:keys [ip] :as new-state}
              (execute instr-fix state)]
          ;; if loop detected, restart from last branch
          (if (contains? ips-seen ip)
            (recur (:state new-branch) (:seen new-branch) nil true)
            (recur new-state (conj ips-seen ip) new-branch false)))))))

(comment

  ;; solution 1
  (with-open [rdr (io/reader "resources/eight.data")]
    (->> rdr
      line-seq
      (mapv parse-instruction)
      (detect-loop))) ;; => 1451

  ;; solution 2
  (with-open [rdr (io/reader "resources/eight.data")]
    (->> rdr
      line-seq
      (mapv parse-instruction)
      (fix-loop))) ;;

  )
