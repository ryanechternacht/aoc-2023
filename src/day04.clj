(ns day04
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def sample-file "resources/day04-sample.txt")
(def input-file "resources/day04.txt")

(defn read-lines [file]
  (->> file
       slurp
       str/split-lines))

(defn parse-line-sample [line]
  (let [numbers (re-seq #"\d+" line)]
    {:num (->> numbers first parse-long)
     :winners (->> numbers (drop 1) (take 5) (into #{}))
     :mine (->> numbers (drop 6) (into #{}))}))

(defn parse-line-input [line]
  (let [numbers (re-seq #"\d+" line)]
    {:num (->> numbers first parse-long)
     :winners (->> numbers (drop 1) (take 10) (into #{}))
     :mine (->> numbers (drop 11) (into #{}))}))

(defn determine-score [{:keys [winners mine]}]
  (->> mine
       (set/intersection winners)
       count
       dec
       (Math/pow 2)
       int))

(comment
  (parse-line-sample "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")
  (determine-score {:winners #{"83" "17" "48" "41" "86"}, :mine #{"83" "86" "6" "31" "17" "9" "48" "53"}})
  (determine-score {:winners #{}, :mine #{}})

  (parse-line-input "Card   1: 33 56 23 64 92 86 94  7 59 13 | 86 92 64 43 10 70 16 55 79 33 56  8  7 25 82 14 31 96 94 13 99 29 69 75 23")
  ;
  )

;; p1
(->> input-file
     read-lines
     (map parse-line-input)
     (map determine-score)
     (reduce +))

;; p2
(defn determine-wins [{:keys [winners mine]}]
  (->> mine
       (set/intersection winners)
       count))

(defn get-extra-ticket-numbers [ticket-num win-count]
  (->> win-count
       range
       (map #(+ ticket-num 1 %))))

(defn add-wins [ticket-counts new-ticket-nums num-of-plays]
  (reduce (fn [acc i]
            (update acc i #(+ num-of-plays %)))
          ticket-counts
          new-ticket-nums))

(comment 
  (determine-wins {:winners #{"83" "17" "48" "41" "86"}, :mine #{"83" "86" "6" "31" "17" "9" "48" "53"}})
  (get-extra-ticket-numbers 3 2)
  (add-wins {0 1, 1 1, 2 1, 3 1, 4 1, 5 1} '(4 5) 3)
  ;
  )

(let [scores (->> input-file
                  read-lines
                  (map parse-line-input)
                  (map determine-wins)
                  vec)
      ticket-counts (->> scores
                         count
                         range
                         (map-indexed (fn [i _] [i 1]))
                         (into {}))
      final-tickets (reduce-kv (fn [acc k v]
                                 (let [starting-tickets (acc k)]
                                   (add-wins acc (get-extra-ticket-numbers k v) starting-tickets)))
                               ticket-counts
                               scores)]
  (->> final-tickets
       (map second)
       (reduce +)))
