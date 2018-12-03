(ns d01)

(require '[help])

(def input (map (fn [i] (Integer/parseInt i)) (help/input "01.txt")))

(reduce + 0 input)

(->> input
     cycle
     (reductions + 0)
     (reduce #(if (contains? %1 %2) (reduced %2) (conj %1 %2)) #{}))
