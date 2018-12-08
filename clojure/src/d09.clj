(ns d08
  (:require [clojure.zip :as zip]))

(require '[help])
(require '[clojure.string :as str])
(require '[clojure.zip :as zip])

(def input (help/input "08.txt"))
(def nums (map #(Integer/parseInt %) (str/split (first input) #"\s+")))

(def tree
  (loop [c (-> [{:cc (first nums) :mc (second nums)}]
               zip/vector-zip
               zip/down)
         remaining (drop 2 nums)]
    (if (empty? remaining)
      (zip/root c)
      (let [{:keys [cc mc]} (some-> c zip/leftmost zip/node)]
        (if (> cc (some-> c zip/up zip/children count (- 1)))
          (let [[next-child-count next-meta-count & next-remaining] remaining]
            (recur (-> c
                       (zip/insert-right
                        [{:cc next-child-count
                          :mc next-meta-count}])
                       (zip/right)
                       (zip/down))
                   next-remaining))
          (recur (-> c
                     (zip/leftmost)
                     (zip/edit assoc :meta (take mc remaining))
                     (zip/edit (fn [node]
                                 (if (zero? (:cc node))
                                   (assoc node :value (reduce + (:meta node)))
                                   (let [parent (-> c zip/up zip/node)]
                                     (->> (take mc remaining)
                                          (remove zero?)
                                          (map #(get parent %))
                                          (filter identity)
                                          (map (comp :value first))
                                          (reduce + 0)
                                          (assoc node :value))))))
                     (zip/up))
                 (drop mc remaining)))))))

;; part 1
(->> tree
     flatten
     (filter :meta)
     (mapcat :meta)
     (reduce +))

;; part 2
(-> tree first :value)
