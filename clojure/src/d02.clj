(ns d02)

(require '[help])

(def input (into (list) (help/input "02.txt")))

(->> input
     (map (comp frequencies vals frequencies))
     (reduce (fn incr-similar [[twos threes] x]
               (let [has-pos? #(pos? (or (get x %) 0))]
                 [(if (has-pos? 2) (inc twos) twos)
                  (if (has-pos? 3) (inc threes) threes)]))
             [0 0])
     (apply *))

(->> input
     (reductions conj (list))
     (mapcat (fn make-pairs [[head & etc]]
               (map (fn make-pair [s] [head s]) etc)))
     (map (fn zip-chars [[this that]]
            (map list this that)))
     (remove (fn find-similar [pairs]
               (->> pairs
                    (remove (partial apply =))
                    (drop 1)
                    not-empty)))
     first
     (filter (partial apply =))
     (map first)
     (apply str))
