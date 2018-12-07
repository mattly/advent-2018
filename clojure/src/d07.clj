(ns d07
  (:require [clojure.set :as set]))

(require '[help])
(require '[clojure.set :as set])

(def graph
  (reduce (fn parse-step-line [steps line]
            (let [dep (subs line 5 6)
                  step (subs line 36 37)]
              (update steps step (fnil conj #{}) dep)))
          {}
          (help/input "07.txt")))

;; part 1
(loop [order []
       remaining (apply set/union (set (keys graph)) (vals graph))
       punting #{}]
  (let [candidate (->> remaining (remove punting) sort first)]
    (cond
      (nil? candidate) (apply str order)

      (some remaining (get graph candidate))
      (recur order remaining (conj punting candidate))

      :else
      (recur (conj order candidate) (disj remaining candidate) #{}))))

;; part 2
(defn process-completed [{:as state :keys [time]}]
  (reduce (fn complete-workers [next worker]
            (if (= time (:done-at worker))
              (-> next
                  (update :done conj (:processing worker))
                  (update :workers conj {}))
              (update next :workers conj worker)))
          (assoc state :workers [])
          (:workers state)))

(defn process-idle [{:as state :keys [time done workers remaining]}]
  (if-let [candidates (not-empty (->> remaining sort (filter #(set/subset? (get graph % #{}) done))))]
    (reduce (fn process-worker [next worker]
              (if (empty? worker)
                (let [[this & etc] (:candidates next)]
                  (if this
                    (-> next
                        (update :remaining disj this)
                        (assoc :candidates etc)
                        (update :workers conj {:processing this
                                               :done-at (+ time -4 (int (first (into (list) this))))}))
                    (update next :workers conj {})))
                (update next :workers conj worker)))
            (merge state {:workers []
                          :candidates candidates})
            workers)
    state))

(let [steps (apply set/union (set (keys graph)) (vals graph))]
  (loop [state
         {:time -1
          :remaining steps
          :done #{}
          :workers [{} {} {} {} {}]}]
    (let [next (-> state
                   (update :time inc)
                   process-completed
                   process-idle)]
      (if (or (and (empty? (:remaining next))
                   (empty? (remove empty? (:workers next))))
              (< 1000000 (:time state)))
        next
        (recur next)))))
