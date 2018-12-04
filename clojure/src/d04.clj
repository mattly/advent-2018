(ns d04
  (:require [clojure.string :as str]))

(require '[help])
(require '[clojure.string :as str])

(def input
  (sort (help/input "04.txt")))

(defn minute [line] (Integer/parseInt (subs line 15 17)))

(def sleepy-guard
  (->> input
       (reduce (fn [[guard-naps active-guard nap-started] line]
                 (cond
                   (str/ends-with? line "begins shift")
                   [guard-naps (re-find #"#\d+" line)]

                   (str/ends-with? line "falls asleep")
                   [guard-naps active-guard (minute line)]

                   (str/ends-with? line "wakes up")
                   [(apply update guard-naps active-guard (fnil conj (list)) (range nap-started (minute line)))
                    active-guard]))
               [{}])
       first
       (sort-by (comp count second))
       last))

(* (-> sleepy-guard first (subs 1) Integer/parseInt)
   (->> sleepy-guard second frequencies (sort-by val) last key))
