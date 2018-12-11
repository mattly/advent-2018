(ns d11)

(require '[clojure.set :as set])
(def serial-num 5177)

(defn hund [x] (mod (int (/ x 100)) 10))
(def cells
  (mapv (fn [x]
          (let [rack-id (+ x 10)]
            (mapv (fn [y]
                    {:x x :y y
                     :power (-> rack-id (* y) (+ serial-num) (* rack-id) hund (- 5))})
                  (range 1 301))))
        (range 1 301)))

(defn region-power [{:as cell cx :x cy :y} size]
  (let [others (for [x (range size) y (range size)]
                 (get-in cells [(+ x cx -1) (+ y cy -1)]))]
    (merge cell {:region (->> others (filter identity) (map :power) (reduce +))
                 :size size})))

(def with-region
  (->> cells
       flatten
       (map #(region-power % 3))
       (sort-by :region)))

(println (last with-region))

(def with-sized-region
  (->> cells flatten
       (mapcat (fn [{:as c :keys [x y]}]
                 (println x y)
                 (for [size (range 1 30)]
                   (region-power c size))))
       (sort-by :region)))

(println (last with-sized-region))
