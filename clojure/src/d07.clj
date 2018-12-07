(ns d07
  (:require [clojure.set :as set]))

(require '[help])
(require '[clojure.set :as set])

(def graph
  (reduce (fn parse-step-line [steps line]
            (let [dep (subs line 5 6)
                  step (subs line 36 37)]
              (update steps step (fnil conj #{}) dep)))
          {}
          (help/input "07.txt")))

;; part 1
(loop [order []
       remaining (apply set/union (set (keys graph)) (vals graph))
       punting #{}]
  (let [candidate (->> remaining (remove punting) sort first)]
    (cond
      (nil? candidate) (apply str order)

      (some remaining (get graph candidate))
      (recur order remaining (conj punting candidate))

      :else
      (recur (conj order candidate) (disj remaining candidate) #{}))))

;; part 2
