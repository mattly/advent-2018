(ns d08
  (:require [clojure.zip :as zip]))

(require '[help])
(require '[clojure.string :as str])
(require '[clojure.zip :as zip])

(def input (help/input "08.txt"))
(def nums (map #(Integer/parseInt %) (str/split (first input) #"\s+")))

(def tree
  (loop [c (-> [{:child-count (first nums) :meta-count (second nums)}]
               zip/vector-zip
               zip/down)
         remaining (drop 2 nums)]
    (if (empty? remaining)
      (zip/root c)
      (let [{:keys [child-count meta-count]} (some-> c zip/leftmost zip/node)]
        (if (> child-count (some-> c zip/up zip/children count (- 1)))
          (let [[next-child-count next-meta-count & next-remaining] remaining]
            (recur (-> c
                       (zip/insert-right
                        [{:child-count next-child-count
                          :meta-count next-meta-count}])
                       (zip/right)
                       (zip/down))
                   next-remaining))
          (recur (-> c
                     (zip/insert-right {:meta (take meta-count remaining)})
                     (zip/up))
                 (drop meta-count remaining)))))))

;; part 1
(->> tree
     flatten
     (filter :meta)
     (mapcat :meta)
     (reduce +))
