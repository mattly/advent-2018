(ns d05)

(require '[help])
(require '[clojure.string :as str])

(def polymer (first (help/input "05.txt")))
(count polymer)

(defn can-eliminate? [[c1 c2]]
  (and c1 c2
       (not= c1 c2)
       (= (str/upper-case c1) (str/upper-case c2))))

(loop [processed []
       remaining (into (list) polymer)
       tripped? false]
  (cond
    (> 1 (count remaining))
    (if tripped?
      (recur [] processed false)
      (count processed))

    (can-eliminate? (take 2 remaining))
    (recur processed (drop 2 remaining) true)

    :else (recur (conj processed (first remaining))
                 (rest remaining)
                 tripped?)))
