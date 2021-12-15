(ns advent-day4)

(def boards (read-string (slurp "2021/data/day4-boards.edn")))
(def numbers (read-string (slurp "2021/data/day4-report.edn")))


(defn partition-boards [coll]
  (map #(partition 5 %) (partition 25 coll)))


(defn get-boards [coll]
  (let [lines (partition-boards coll)
        columns (map #(apply mapv vector %) (partition-boards coll))]
    (map (fn [c1 c2] (reduce conj c1 c2)) lines columns)))


(defn remove-number-from-boards [number boards]
  (map (fn [board] (filter #(not (= number %)) board)) boards))


(defn get-bingo-board-id [boards]
  (keys (filter
   #(= (val %) true)
   (reduce-kv (fn [res idx itm] (assoc res idx itm)) 
              {}
              (vec (map #(contains? (set (map empty? %)) true) boards))))))


(defn get-board-sum [board]
  (/ (reduce + (flatten board)) 2))


(defn run-bingo [coll random-numbers last-board?]
    (loop [n 0
           boards (get-boards coll)]
      (cond
        (and (not (empty? (get-bingo-board-id boards))) (= 1 (count boards)))
        (* (random-numbers (dec n)) (get-board-sum boards))

        (and last-board? (not (empty? (get-bingo-board-id boards))))
        (recur n
               (concat (subvec (vec boards)
                               0 (-> boards get-bingo-board-id first))
                       (subvec (vec boards)
                               (-> boards get-bingo-board-id first inc))))

        (not (empty? (get-bingo-board-id boards)))
        (* (random-numbers (dec n)) (get-board-sum (nth boards (first (get-bingo-board-id boards)))))

        :default
        (recur (inc n)
               (map (partial remove-number-from-boards (random-numbers n)) boards)))))


(println "Part 1:" (run-bingo boards numbers false))
(println "Part 2:" (run-bingo boards numbers true))
