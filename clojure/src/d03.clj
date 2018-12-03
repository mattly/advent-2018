(ns d03)

(require '[help])
(require '[clojure.string :as str])

(def input
  (map (fn parse-line-to-claim [line]
         (->> (str/split line #"[^0-9]+")
              (filter not-empty)
              (map (fn parse-nums [i] (Integer/parseInt i)))
              (zipmap [:id :x :y :w :h])))
       (help/input "03.txt")))

;; validating -- what are our max coordinates?
(->> input (map #(+ (:x %) (:w %))) (apply max))
(->> input (map #(+ (:y %) (:h %))) (apply max))

;; find contested coordinates
(->> input
     (mapcat (fn coordinates-for [{:keys [x y w h]}]
               (for [xp (range w)
                     yp (range h)]
                 [(+ x xp) (+ y yp)])))
     frequencies
     vals
     (filter (fn find-contested [freq] (< 1 freq)))
     count)
