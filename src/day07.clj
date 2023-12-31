(ns day07
  (:require [clojure.string :as str]))

(def sample-file "resources/day07-sample.txt")
(def input-file "resources/day07.txt")

(defn read-lines [file]
  (->> file
       slurp
       str/split-lines))

(def card-map
  {\2 2
   \3 3
   \4 4
   \5 5
   \6 6
   \7 7
   \8 8
   \9 9
   \T 10
   \J 11
   \Q 12
   \K 13
   \A 14})

(defn parse-line [line card-map]
  (let [[hand bet] (str/split line #"\s+")]
    {:hand (map card-map hand)
     :original-hand hand
     :bet (parse-long bet)}))

(defn add-hand-strength [hand]
  (let [[c1 c2] (->> hand
                     frequencies
                     (map second)
                     sort
                     reverse)
        hand-strength
        (cond
          (= c1 5) 7
          (= c1 4) 6
          (and (= c1 3) (= c2 2)) 5
          (= c1 3) 4
          (= c1 c2 2) 3
          (= c1 2) 2
          :else 1)]
    (vec (cons hand-strength hand))))

(comment 
  (str/split "32T3K 765" #"\s+")
  (parse-line "32T3K 765" card-map)
  (add-hand-strength '(1 1 1 1 1))
  (add-hand-strength '(1 1 3 1 1))
  (add-hand-strength '(1 1 3 1 3))
  (add-hand-strength '(1 1 1 2 3))
  (add-hand-strength '(2 3 3 1 2))
  (add-hand-strength '(1 3 4 5 4))
  (add-hand-strength '(3 1 6 2 4))
  ;
  )

;; p1
(->> input-file
     read-lines
     (map #(parse-line % card-map))
     (map #(update % :hand add-hand-strength))
     (sort-by :hand)
     (map-indexed (fn [i {:keys [bet]}]
                    (* (inc i) bet)))
     (reduce +))

(def card-map-2
  {\2 2
   \3 3
   \4 4
   \5 5
   \6 6
   \7 7
   \8 8
   \9 9
   \T 10
   \J 1 ;; intentional
   \Q 12
   \K 13
   \A 14})

(defn add-hand-strength-2 [hand]
  (let [freqs (frequencies hand)
        js (freqs 1 0)
        [c1 c2] (->> (dissoc freqs 1)
                     (map second)
                     sort
                     reverse)
        c1 (if-not (nil? c1) 
             (+ c1 js)
             5)
        hand-strength
        (cond
          ;; (nil? c1) 7 ;; all 1's
          (= c1 5) 7
          (= c1 4) 6
          (and (= c1 3) (= c2 2)) 5
          (= c1 3) 4
          (= c1 c2 2) 3
          (= c1 2) 2
          :else 1)]
    (vec (cons hand-strength hand))))

(comment
  (parse-line "3JJ3K 765" card-map-2)
  (add-hand-strength-2 '(3 1 1 3 13))
  (add-hand-strength-2 '(3 1 1 3 3))
  (add-hand-strength-2 '(4 1 1 2 3))
  (add-hand-strength-2 '(1 1 2 3 4))
  (add-hand-strength-2 '(4 1 2 3 6))
  (add-hand-strength-2 '(2 3 4 5 6))
  (add-hand-strength-2 '(1 1 1 1 1))
  (add-hand-strength-2 '(1 1 2 3 4))
  ;
  )

;; p2
(->> input-file
     read-lines
     (map #(parse-line % card-map-2))
     (map #(update % :hand add-hand-strength-2))
     (sort-by :hand)
     (map-indexed (fn [i {:keys [bet]}]
                    (* (inc i) bet)))
     (reduce +))
