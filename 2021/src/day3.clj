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

(def get-gamma-bit #(->> % frequencies (sort-by val) last key))

(def get-epsilon-bit #(->> % frequencies (sort-by val) first key))

(defn get-power-rate [report-parsed fn-rate-selector]
  (Integer/parseInt
   (str/join
    (map fn-rate-selector report-parsed)) 2))

(defn calc-power-consumption [report]
  (* (get-power-rate report get-gamma-bit) 
     (get-power-rate report get-epsilon-bit)))

(println
 "Power consumption:"
 (calc-power-consumption (parse-input all-records)))


; part 2
(defn get-oxygen-bit [col report]
  (->> (map #(get % col) report)
       frequencies
       (sort-by (juxt val key)) last key))

(defn get-co2-bit [col report]
  (->> (map #(get % col) report)
       frequencies
       (sort-by (juxt val key)) reverse last key))

(defn get-life-support-rate [report fn-rate-selector]
  (loop [cnt 0
         acc report]
    (let [most-commom (fn-rate-selector cnt acc)]
      (if (= (count acc) 1)
        (Integer/parseInt (apply str acc) 2)
        (recur (inc cnt) (filter #(= (get % cnt) most-commom) acc))))))

(defn calc-life-support [report]
  (* (get-life-support-rate report get-oxygen-bit) 
     (get-life-support-rate report get-co2-bit)))

(println "Life support rating:"
         (calc-life-support all-records))
