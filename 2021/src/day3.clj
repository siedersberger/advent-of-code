(ns advent-day3
  (:require [clojure.string :as str]))


(def all-records (str/split (slurp "2021/data/day3-report.edn") #"\n"))

(def example
  ["00100"
   "11110"
   "10110"
   "10111"
   "10101"
   "01111"
   "00111"
   "11100"
   "10000"
   "11001"
   "00010"
   "01010"])


; part 1
(defn parse-input [report]
  (apply mapv vector
         (map #(str/split % #"") report)))

(defn get-gamma-rate [report-parsed]
  (Integer/parseInt
   (str/join
    (map #(->> % frequencies (sort-by val) last key)
         report-parsed)) 2))

(defn get-epsilon-rate [report-parsed]
   (Integer/parseInt
    (str/join
     (map #(->> % frequencies (sort-by val) first key)
          report-parsed)) 2))

(defn calc-power-consumption [report]
  (* (get-epsilon-rate report) (get-gamma-rate report)))

(println
 "Power consumption:"
 (calc-power-consumption (parse-input all-records)))


; part 2
(defn get-oxygen-rate [report]
  (loop [cnt 0
         acc report]
    (let [most-commom (->> (map #(get % cnt) acc)
                           frequencies
                           (sort-by (juxt val key)) last key)]
      (if (= (count acc) 1)
        (Integer/parseInt (apply str acc) 2)
        (recur (inc cnt) (filter #(= (get % cnt) most-commom) acc))))))

(defn get-co2-rate [report]
  (loop [cnt 0
         acc report]
    (let [most-commom (->> (map #(get % cnt) acc)
                           frequencies
                           (sort-by (juxt val key)) reverse last key)]
      (if (= (count acc) 1)
        (Integer/parseInt (apply str acc) 2)
        (recur (inc cnt) (filter #(= (get % cnt) most-commom) acc))))))

(defn calc-life-support [report]
  (* (get-co2-rate report) (get-oxygen-rate report)))

(println "Life support rating:"
         (calc-life-support all-records))
