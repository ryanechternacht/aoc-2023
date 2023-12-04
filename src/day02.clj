(ns day02
  (:require [clojure.string :as str]))

(def sample-file "resources/day02-sample.txt")
(def input-file "resources/day02.txt")
(def max-cubes {:red 12 :green 13 :blue 14})

(defn read-lines [file]
  (->> file
       slurp
       str/split-lines))

(defn parse-game [line]
  (let [chunks (str/split line #"[:;]")
        id (parse-long (re-find #"\d+" (first chunks)))
        pulls (map #(->> %
                         (re-seq #"\d+|\w+")
                         (partition 2)
                         (map (fn [[n c]]
                                {:num (parse-long n)
                                 :color c})))
                   (rest chunks))]
    {:id id
     :pulls pulls}))

(defn find-max-needed [{:keys [id pulls]}]
  (let [max-possible (reduce (fn [acc {:keys [num color]}]
                               (condp = color
                                 "green" (update acc :green max num)
                                 "blue" (update acc :blue max num)
                                 "red" (update acc :red max num)
                                 acc))
                             {:red 0 :green 0 :blue 0}
                             (flatten pulls))]
    (assoc max-possible :id id)))

(defn is-game-possible? [{max-r :red max-b :blue max-g :green}
                         {:keys [red green blue]}]
  (and (<= red max-r)
       (<= blue max-b)
       (<= green max-g)))

(comment
  (parse-game "Game 123: 3 blue, 4 red; 1 red, 4 green, 6 blue; 2 green")
  (find-max-needed
   {:id 123,
    :pulls
    '(({:num 3, :color "blue"} {:num 4, :color "red"})
      ({:num 1, :color "red"} {:num 4, :color "green"} {:num 6, :color "blue"})
      ({:num 2, :color "green"}))})

  (is-game-possible? 
   {:red 1 :green 4 :blue 8}
   {:red 2, :green 4, :blue 6, :id 123})
  ;
  )

;; p1
(->> input-file
     read-lines
     (map parse-game)
     (map find-max-needed)
     (filter #(is-game-possible? max-cubes %))
     (map :id)
     (reduce +))

(defn find-power [{:keys [red blue green]}]
  (* red blue green))

(comment
  (find-power {:red 2, :green 4, :blue 6, :id 123})
  )

;; p2
(->> input-file
     read-lines
     (map parse-game)
     (map find-max-needed)
     (map find-power)
     (reduce +))