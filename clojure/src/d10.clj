(ns d10)

(require '[help])
(require '[clojure.string :as s])

(defn extract-int [line start end]
  (Integer/parseInt (s/trim (subs line start end))))

(def points
  (->> (help/input "10.txt")
       (map (fn [line]
              {:px (extract-int line 10 16)
               :py (extract-int line 17 24)
               :vx (extract-int line 36 38)
               :vy (extract-int line 40 42)}))))

(loop [pts points
       small? false
       iter 0]
  (if (< 1000000 iter)
    nil
    (let [next-pts (map (fn [{:as pt :keys [vx vy]}]
                          (-> pt (update :px + vx) (update :py + vy)))
                        pts)
          ys (map :py next-pts)
          height (- (apply max ys) (apply min ys))
          sm? (> 30 height)]
      (when (zero? (mod iter 100))
        (println iter height))
      (when sm?
        (newline)
        (println iter)
        (let [xs (map :px next-pts)
              top-row (apply min ys)
              bot-row (apply max ys)
              lt-col (apply min xs)
              rt-col (apply max xs)]
          (doall (for [row (range top-row (inc bot-row))]
                   (let [row-pts (->> next-pts
                                      (filter #(= row (:py %)))
                                      (map :px)
                                      set)]
                     (println
                      (reduce (fn [s p]
                                (str s (if (contains? row-pts p) "*" " ")))
                              ""
                              (range lt-col (inc rt-col)))))))))
      (if (and small? (not sm?))
        nil
        (recur next-pts sm? (inc iter))))))
