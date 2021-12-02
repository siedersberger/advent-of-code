(ns advent-day1)

(def example [199 200 208 210 200 207 240 269 260 263])
(def all-records (read-string (slurp "data/day1-report.edn")))


(defn slide-window [measurements]
  (reduce 
  (fn [acc value] 
    (conj acc (apply + value))) 
   []
   (partition 3 1 measurements)))


(defn count-increases [report]
  (count (filter
   (fn [x] (= (second x ) :increase))
   (reduce
    (fn [acc val]
      (cond
        (empty? acc)
        [[val :no-previous-measurement]]

        (> val (-> acc last first))
        (conj acc [val :increase])

        :default
        (conj acc [val :decrease])))
    []
    report))))

;; part 1
(count-increases example)
(count-increases all-records)

;; part 2
(count-increases (slide-window example))
(count-increases (slide-window all-records))