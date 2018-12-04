(ns d04)

(require '[help])
(require '[clojure.string :as str])

(defn minute [line] (Integer/parseInt (subs line 15 17)))

;; part 1
(def sleepy-guards
  (->> (help/input "04.txt")
       sort
       (reduce (fn [[guard-naps active-guard nap-started] line]
                 (cond
                   (str/ends-with? line "begins shift")
                   [guard-naps (-> (re-find #"#\d+" line) (subs 1) Integer/parseInt)]

                   (str/ends-with? line "falls asleep")
                   [guard-naps active-guard (minute line)]

                   (str/ends-with? line "wakes up")
                   [(apply update guard-naps active-guard (fnil conj (list)) (range nap-started (minute line)))
                    active-guard]))
               [{}])
       first))

(let [[id mins] (->> sleepy-guards (sort-by (comp count second)) last)]
  (* id (->> mins frequencies (sort-by val) last key)))

;; part 2
(->> sleepy-guards
     (map #(update % 1 frequencies))
     (sort-by (comp (partial apply max) vals second))
     (take-last 1)
     (map #(update % 1 (comp key last (partial sort-by val))))
     first
     (apply *))
