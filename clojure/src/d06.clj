(ns d06)

(require '[help])
(require '[clojure.string :as str])

(def points (->> (help/input "06.txt")
                 (map #(str/split % #"[^\d]+"))
                 (map (partial map #(Integer/parseInt %)))))

(def max-x (->> points (map first) (apply max)))
(def max-y (->> points (map second) (apply max)))
(def edge-x #{0 max-x})
(def edge-y #{0 max-y})

(defn calc-dist [lx ly [px py]]
  (+ (Math/abs (- px lx)) (Math/abs (- py ly))))

(def locations
  (for [x (range (inc max-x))
        y (range (inc max-y))]
    (let [distances (map (juxt identity (partial calc-dist x y)) points)
          nearest
          (->> distances
               (sort-by second)
               (partition-by second)
               first
               (map first))]
      {:x x :y y
       :nearest nearest
       :conflicted? (< 1 (count nearest))
       :edge? (or (edge-x x) (edge-y y))
       :total (->> distances (map second) (reduce +))})))

;; first
(->> locations
     (mapcat (fn [{:keys [nearest conflicted? edge?]}]
               (map (fn [pt] {:pt pt :conflicted? conflicted? :edge? edge?}) nearest)))
     (group-by :pt)
     (remove (fn [[_ locs]] (not-empty (filter :edge? locs))))
     (map (fn [[pt locs]] [pt (count (remove :conflicted? locs))]))
     (sort-by second)
     (last))

;;second
(count (filter #(>= 10000 (:total %)) locations))
