(ns aoc.d01)

(require '[clojure.java.io :as io])
(require '[clojure.string :as str])

(def input (->> "01.txt" io/resource slurp str/split-lines (map #(Integer/parseInt %))))

(reduce + 0 input)

(->> input
     cycle
     (reductions + 0)
     (reduce #(if (contains? %1 %2) (reduced %2) (conj %1 %2)) #{}))
