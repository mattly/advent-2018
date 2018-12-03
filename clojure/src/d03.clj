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
