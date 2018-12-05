(ns d05)

(require '[help])
(require '[clojure.string :as str])

(def polymer (into (list) (first (help/input "05.txt"))))

(defn can-eliminate? [[c1 c2]]
  (and c1 c2 (or (= (- (int c1) 32) (int c2))
                 (= (- (int c2) 32) (int c1)))))

(defn process [chain]
  (loop [processed []
         remaining chain
         tripped? false]
    (cond
      (> 1 (count remaining))
      (if tripped?
        (recur [] processed false)
        processed)

      (can-eliminate? (take 2 remaining))
      (recur processed (drop 2 remaining) true)

      :else
      (recur (conj processed (first remaining))
             (rest remaining)
             tripped?))))

(count (process polymer))

(->> (range 65 91)
     (map (juxt identity (comp count process #(remove #{(char %) (char (+ % 32))} polymer))))
     (sort-by second)
     first)
