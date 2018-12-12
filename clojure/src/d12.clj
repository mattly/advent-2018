(ns d12)

(require '[help])
(require '[clojure.string :as str])
(def input (help/input "12.txt"))

(def initial-state (->> (str/replace (first input) #"initial state: " "")
                        (into (list))
                        reverse
                        (map (fn [i c] [i c])
                             (range))
                        (into {})))
(println initial-state)

(defn print-state [s]
  (let [ks (sort (keys s))]
    (->> ks
         (map #(get s %))
         (apply str (first ks) " "))))

(def the-rules
  (->> (drop 2 input)
       (map (fn [line]
              (str/split line #"\s=>\s")))
       (into {})))
(clojure.pprint/pprint the-rules)

(defn process [state rules gen]
  (when (zero? (mod gen 1000000)) (println gen))
  (->> state
       keys
       sort
       ((fn pad-keys [ks]
          (let [start (first ks)
                end (last ks)]
            (conj ks (- start 1) (- start 2) (+ end 1) (+ end 2)))))
       (map (fn [k]
              [k (get rules (reduce #(str %1 (get state %2 ".")) "" (range (- k 2) (+ k 3))))]))
       (partition-by second)
       ((fn remove-dead-ends [segments]
          (->> segments
               (drop (if (= "." (second (first segments))) 1 0))
               (drop-last (if (= "." (second (last segments))) 1 0)))))
       (mapcat identity)
       (into {})))

(process initial-state the-rules 1)

(def last-sum (atom 0))
(def diffs (atom (list)))

(defn calc-to-gen [gen]
  (->> (range gen)
       (reduce (fn process-gen [state i]
                 (let [s (process state the-rules i)
                       sum (->> s (filter (fn alive? [[_ v]] (= v "#"))) (map first) (reduce +))
                       diff (- sum @last-sum)]
                   (when (every? #(= diff %) @diffs)
                     (println i sum diff))
                   (reset! last-sum sum)
                   (swap! diffs (fn [d] (take 5 (conj d diff))))
                   s))
               initial-state)
       (filter (fn [[_ v]] (= v "#")))
       (map first)
       (reduce +)))

(println (calc-to-gen 20))
(println (calc-to-gen 2000))
(println (calc-to-gen 50000000000))
(println (+ 184111 (* 91 (- 50000000000 2000))))
