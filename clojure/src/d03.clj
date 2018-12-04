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

;; find contested coordinates
(->> input
     (mapcat (fn coordinates-for-claim [{:keys [x y w h]}]
               (for [xp (range w)
                     yp (range h)]
                 [(+ x xp) (+ y yp)])))
     frequencies
     vals
     (filter (fn find-contested [freq] (< 1 freq)))
     count)

;; find uncontested claim
(->> input
     (mapcat (fn claims-by-coord [{:keys [id x y w h]}]
               (for [px (range w)
                     py (range h)]
                 {:id id :coord [(+ x px) (+ y py)]})))
     (reduce (fn claim-ids-by-coord [m {:keys [id coord]}]
               (update m coord (fnil conj #{}) id))
             {})
     vals
     (reduce (fn reject-competinng-claims [[candidates rejects] claims-at-coord]
               (if (or (some rejects claims-at-coord) (< 1 (count claims-at-coord)))
                 [(apply disj candidates claims-at-coord) (apply conj rejects claims-at-coord)]
                 [(conj candidates (first claims-at-coord)) rejects]))
             [#{} #{}])
     first)
