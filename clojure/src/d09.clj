(ns d09)

(def players 439)
(def marbles 7130700)

(require '[clojure.zip :as zip])

(-> [0] zip/vector-zip zip/down)
(defn move-or-wrap [loc' dir times]
  (reduce (fn [loc _]
            (case dir
              :left (or (zip/left loc) (zip/rightmost loc))
              :right (or (zip/right loc) (zip/leftmost loc))))
          loc'
          (range times)))

(->> (range marbles)
     (map inc)
     (reduce (fn [{:as state :keys [board]} marble]
               (if (-> marble (mod 23) zero?)
                 (let [loc-to-take (move-or-wrap board :left 7)]
                   (-> state
                       (update-in [:players (mod marble players)] (fnil conj #{}) marble (zip/node loc-to-take))
                       (assoc :board (-> loc-to-take zip/remove (move-or-wrap :right 1)))))
                 (assoc state :board (-> board (move-or-wrap :right 1) (zip/insert-right marble) zip/right))))
             {:board (zip/down (zip/vector-zip [0]))
              :players {}})
     :players
     vals
     (map (partial reduce +))
     (apply max))
