(ns day06
  (:require [clojure.string :as str]))

(def sample-file "resources/day06-sample.txt")
(def input-file "resources/day06.txt")

(defn read-data [file]
  (->> file
       slurp
       str/split-lines
       (map #(re-seq #"\d+" %))
       (apply zipmap)
       (map (fn [[k v]]
              {:race-length (parse-long k)
               :best-score (parse-long v)}))))

(defn build-potential-scores [race-length]
  (->> race-length
       range
       (map (juxt identity #(- race-length %)))
       (map #(apply * %))))

(defn determine-winning-options [{:keys [race-length best-score]}]
  (->> race-length
       build-potential-scores
       (filter #(> % best-score))
       count))

(comment
  (build-potential-scores 7)
  (determine-winning-options {:race-length 7, :best-score 9})
  ;
  )

(->> input-file
     read-data
     (map determine-winning-options)
     (reduce *))