(ns day01
  (:require [clojure.string :as str]))

(def sample-file "resources/day01-sample.txt")
(def sample-file-2 "resources/day01-sample-2.txt")
(def input-file "resources/day01.txt")

(defn read-lines [file]
  (->> file
       slurp
       str/split-lines))

(defn find-first-digit [str]
  (re-find #"\d" str))

(comment
  (find-first-digit "abc123")
  (find-first-digit "34abc123")
  ((juxt find-first-digit (comp find-first-digit str/reverse)) "abc123")
  ;
  )


(->> input-file
     read-lines
     (map (juxt find-first-digit (comp find-first-digit str/reverse)))
     (map (comp parse-long #(apply str %)))
     (reduce +))

(defn get-digits [str]
  (let [digits? (re-seq #"\d|one|two|three|four|five|six|seven|eight|nine" str)]
    (map #(get {
      "one" "1"
      "two" "2"
      "three" "3"
      "four" "4"
      "five" "5"
      "six" "6"
      "seven" "7"
      "eight" "8"
      "nine" "9"} % %) digits?)))

;; Man this is a lazy hack
(defn get-digits-backwards [str]
  (let [digits? (re-seq #"\d|one|two|three|four|five|six|seven|eight|nine" str)]
    (map #(get {"one" "1"
                "two" "2"
                "three" "3"
                "four" "4"
                "five" "5"
                "six" "6"
                "seven" "7"
                "eight" "8"
                "nine" "9"} % %) digits?)))

(comment
  (get-digits "fourabc123")
  (get-digits "twone")
  (get-digits "4one1eightzgcpkgbpgmsevenninetwonetk")
  ;
  )

(->> sample-file-2
     read-lines
     (map (juxt (comp first get-digits) (comp last get-digits)))
     (map (comp parse-long #(apply str %)))
     (reduce +))
