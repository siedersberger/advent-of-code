(ns advent-day2
  (:require [clojure.string :as str]))

(defn parse-input [raw-report]
  (map
   #(let [splited (str/split % #" ")]
      [(-> splited first keyword) (-> splited second Integer/parseInt)])
   (str/split raw-report #"\n")))

(def all-records (parse-input (slurp "2021/data/day2-report.edn")))

(def example
  [[:forward 5]
   [:down 5]
   [:forward 8]
   [:up 3]
   [:down 8]
   [:forward 2]])


; part1
(defn calc-position [report direction]
  (apply +
         (map
          #(% 1)
          (filter #(= direction (first %)) report))))

(defn compose-positions [report]
  (*
   (- (calc-position report :down) (calc-position report :up))
   (calc-position report :forward)))

(println "Part 1 example input:" (compose-positions example))
(println "Part 1 report input:" (compose-positions all-records))


; part2
(defn calc-position-with-aim [report]
  (reduce
   (fn [acc value]
     (cond
       (= (first value) :down)
       (map + acc [0 0 (second value)])

       (= (first value) :up)
       (map - acc [0 0 (second value)])

       (= (first value) :forward)
       (map +
            (map + acc [(second value) 0 0])
            [0 (* (second value) (last acc)) 0])))
   [0 0 0]
   report))

(defn compose-positions-with-aim [report]
  (let [coordenates (calc-position-with-aim report)]
    (* (first coordenates) (second coordenates))))

(println "Part 2 example input:" (compose-positions-with-aim example))
(println "Part 2 report input:" (compose-positions-with-aim all-records))