(ns day11
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(def sample-file "resources/day11-sample.txt")
(def input-file "resources/day11.txt")

(defn read-lines [file]
  (->> file
       slurp
       str/split-lines))

(defn build-map [lines]
  (reduce-kv (fn [acc y line]
               (reduce-kv (fn [acc2 x c]
                            (condp = c
                              \# (conj acc2 [x y])
                              \. acc2))
                          acc
                          (vec line)))
             #{}
             (vec lines)))

(defn- find-missing [m]
  (let [max-num (->> m sort last)
        all-rows-set (->> max-num range (into #{}))
        rows-with-data-set (into #{} m)]
    (set/difference all-rows-set rows-with-data-set)))

(defn find-empty-rows [m]
  (let [rows-with-data (->> m
                            (map second)
                            distinct)]
    (find-missing rows-with-data)))

(defn find-empty-cols [m]
  (let [cols-with-data (->> m
                            (map first)
                            distinct)]
    (find-missing cols-with-data)))

(defn expand-row [m r]
  (reduce (fn [acc [x y]]
            (if (< r y)
              (conj acc [x (inc y)])
              (conj acc [x y])))
          #{}
          m))

(defn expand-col [m c]
  (reduce (fn [acc [x y]]
            (if (< c x)
              (conj acc [(inc x) y])
              (conj acc [x y])))
          #{}
          m))

(defn expand-map [m empty-rows empty-cols]
  (let [rows-expanded (reduce expand-row
                              m
                              (->> empty-rows sort reverse))
        cols-expanded (reduce expand-col
                              rows-expanded
                              (->> empty-cols sort reverse))]
    cols-expanded))

(defn manhattan-dist [[x1 y1] [x2 y2]]
  (+ (Math/abs (- x1 x2))
     (Math/abs (- y1 y2))))

(defn create-pairs [coll]
  (->> (for [x coll
             y coll]
         [x y])
       (remove (fn [[x y]] (= x y)))))

(comment
  (build-map (read-lines sample-file))
  (find-empty-rows #{[7 1] [7 8] [3 0] [9 6] [4 9] [0 9] [1 5] [6 4] [0 2]})
  (find-empty-cols #{[7 1] [7 8] [3 0] [9 6] [4 9] [0 9] [1 5] [6 4] [0 2]})

  (expand-row #{[7 1] [7 8] [3 0] [9 6] [4 9] [0 9] [1 5] [6 4] [0 2]}
              3)

  (expand-col #{[7 1] [7 8] [3 0] [9 6] [4 9] [0 9] [1 5] [6 4] [0 2]}
              2)

  (expand-map #{[7 1] [7 8] [3 0] [9 6] [4 9] [0 9] [1 5] [6 4] [0 2]}
              #{7 3}
              #{2 5 8})
  
  (manhattan-dist [0 0] [2 3])
  (manhattan-dist [-2 -3] [2 3])
  (manhattan-dist [1 1] [3 2])
  (manhattan-dist [1 1] [3 -2])

  (create-pairs [1 2 3 4])
  ;
  )

(let [m (->> input-file
             read-lines
             build-map)
      empty-rows (find-empty-rows m)
      empty-cols (find-empty-cols m)
      expanded-m (expand-map m empty-rows empty-cols)]
  (->> expanded-m
       create-pairs
       (map #(apply manhattan-dist %))
       (reduce +)
       (#(/ % 2))))

