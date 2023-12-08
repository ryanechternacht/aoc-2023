(ns day05
  (:require [clojure.string :as str]))

(def sample-file "resources/day05-sample.txt")
(def input-file "resources/day05.txt")

(defn read-lines [file]
  (->> file
       slurp
       str/split-lines))

(defn break-lines-into-chunks [lines]
  (loop [ls lines
         chunks []]
    (if (seq ls)
      (recur (rest (drop-while (comp not str/blank?) ls))
             (conj chunks (take-while (comp not str/blank?) ls)))
      chunks)))

(defn build-range-from-line [line]
  (let [[d s r] (re-seq #"\d+" line)]
    {:src-start (parse-long s)
     :dest-start (parse-long d)
     :range (parse-long r)}))

(defn build-chunk [[_ & maps]]
  (map build-range-from-line maps))

(defn build-maps-from-chunks [[seeds & chunks]]
  {:seeds (map parse-long (re-seq #"\d+" (first seeds)))
   :maps (map build-chunk chunks)})

(defn check-value-in-map [{:keys [src-start dest-start range]} value]
  (if (<= src-start value (+ src-start range -1))
    (+ value dest-start (- src-start))
    nil))

(defn map-value [value maps]
  (or (reduce (fn [_ m]
                (if-let [mapped-value (check-value-in-map m value)]
                  (reduced mapped-value)
                  nil))
              nil
              maps)
      value))

(defn map-value-through [maps value]
  (reduce map-value value maps))

(comment
  (break-lines-into-chunks (read-lines sample-file))

  (build-range-from-line "123 456 789")
  (build-chunk '("soil-to-fertilizer map:"
                 "0 15 37"
                 "37 52 2"
                 "39 0 15"))
  (build-maps-from-chunks
   '[("seeds: 79 14 55 13")
     ("seed-to-soil map:" "50 98 2" "52 50 48")
     ("soil-to-fertilizer map:" "0 15 37" "37 52 2" "39 0 15")
     ("fertilizer-to-water map:" "49 53 8" "0 11 42" "42 0 7" "57 7 4")
     ("water-to-light map:" "88 18 7" "18 25 70")
     ("light-to-temperature map:" "45 77 23" "81 45 19" "68 64 13")
     ("temperature-to-humidity map:" "0 69 1" "1 0 69")
     ("humidity-to-location map:" "60 56 37" "56 93 4")])

  (check-value-in-map {:src-start 50 :dest-start 98 :range 2} 49)
  (map-value 79
             [{:src-start 98 :dest-start 50 :range 2}
              {:src-start 50 :dest-start 52 :range 48}])

  (map-value 78
             '({:src-start 56, :dest-start 60, :range 37}
               {:src-start 93, :dest-start 56, :range 4}))

  (map-value-through '(({:src-start 98, :dest-start 50, :range 2} {:src-start 50, :dest-start 52, :range 48})
                       ({:src-start 15, :dest-start 0, :range 37}
                        {:src-start 52, :dest-start 37, :range 2}
                        {:src-start 0, :dest-start 39, :range 15})
                       ({:src-start 53, :dest-start 49, :range 8}
                        {:src-start 11, :dest-start 0, :range 42}
                        {:src-start 0, :dest-start 42, :range 7}
                        {:src-start 7, :dest-start 57, :range 4})
                       ({:src-start 18, :dest-start 88, :range 7} {:src-start 25, :dest-start 18, :range 70})
                       ({:src-start 77, :dest-start 45, :range 23}
                        {:src-start 45, :dest-start 81, :range 19}
                        {:src-start 64, :dest-start 68, :range 13})
                       ({:src-start 69, :dest-start 0, :range 1} {:src-start 0, :dest-start 1, :range 69})
                       ({:src-start 56, :dest-start 60, :range 37} {:src-start 93, :dest-start 56, :range 4}))
                     14)
  ;
  )
;; p1
(let [{:keys [seeds maps]} (->> input-file
                                read-lines
                                break-lines-into-chunks
                                build-maps-from-chunks)]

  (->> seeds
       (map #(map-value-through maps %))
       (reduce min)))

(defn check-value-in-map-backwards [{:keys [src-start dest-start range]} value]
  (if (<= dest-start value (+ dest-start range -1))
    (+ value src-start (- dest-start))
    nil))

(defn map-value-backwards [value maps]
  (or (reduce (fn [_ m]
                (if-let [mapped-value (check-value-in-map-backwards m value)]
                  (reduced mapped-value)
                  nil))
              nil
              maps)
      value))

(defn map-value-through-backwards [maps value]
  (reduce map-value-backwards value (reverse maps)))

(defn build-seed-ranges [seeds]
  (->> seeds
       (partition 2)
       (map (fn [[x y]] {:lower x
                         :upper (+ x y -1)}))
       (sort-by :lower)))

(defn is-seed? [seed-ranges seed?]
  (when (some #(<= (:lower %) seed? (:upper %)) seed-ranges)
    seed?))

(comment
  (check-value-in-map-backwards
   {:src-start 50 :dest-start 98 :range 2}
   50)
  (map-value-backwards 52
                       [{:src-start 98 :dest-start 50 :range 2}
                        {:src-start 50 :dest-start 52 :range 48}])
  (map-value-through-backwards '(({:src-start 98, :dest-start 50, :range 2} {:src-start 50, :dest-start 52, :range 48})
                                 ({:src-start 15, :dest-start 0, :range 37}
                                  {:src-start 52, :dest-start 37, :range 2}
                                  {:src-start 0, :dest-start 39, :range 15})
                                 ({:src-start 53, :dest-start 49, :range 8}
                                  {:src-start 11, :dest-start 0, :range 42}
                                  {:src-start 0, :dest-start 42, :range 7}
                                  {:src-start 7, :dest-start 57, :range 4})
                                 ({:src-start 18, :dest-start 88, :range 7} {:src-start 25, :dest-start 18, :range 70})
                                 ({:src-start 77, :dest-start 45, :range 23}
                                  {:src-start 45, :dest-start 81, :range 19}
                                  {:src-start 64, :dest-start 68, :range 13})
                                 ({:src-start 69, :dest-start 0, :range 1} {:src-start 0, :dest-start 1, :range 69})
                                 ({:src-start 56, :dest-start 60, :range 37} {:src-start 93, :dest-start 56, :range 4}))
                               35)

  (build-seed-ranges '(79 14 55 13))

  (is-seed? '({:lower 55, :upper 67} {:lower 79, :upper 92}) 68)
  ;
  )

(let [{:keys [seeds maps]} (->> input-file
                                read-lines
                                break-lines-into-chunks
                                build-maps-from-chunks)
      seeds (build-seed-ranges seeds)]
  (reduce (fn [_ t]
            (when (zero? (mod t 100000))
              (println t))
            (if-let [seed? (is-seed? seeds (map-value-through-backwards maps t))]
              (reduced seed?)
              nil))
          nil
          (range)))

;; 391178260