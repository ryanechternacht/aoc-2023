(ns day09
  (:require [clojure.string :as str]))

(def sample-file "resources/day09-sample.txt")
(def input-file "resources/day09.txt")

(defn read-lines [file]
  (->> file
       slurp
       str/split-lines))

(defn parse-line [line]
  (->> line
       (re-seq #"[-\d]+")
       (map parse-long)))


(defn get-diff-vec [nums]
  (let [{:keys [new-array]} 
        (reduce (fn [{:keys [last-item] :as acc} next-item]
                  (-> acc
                      (update :new-array conj (- next-item last-item))
                      (assoc :last-item next-item)))
                {:new-array []
                 :last-item (first nums)}
                (rest nums))]
    new-array))

(defn get-next-value [line]
  (loop [diffs [line]
         current line]
    ;; (println diffs current)
    ;; (Thread/sleep 1000)
    (if (every? zero? current)
      (->> diffs
           (map last)
           (reduce +))
      (let [diff (get-diff-vec current)]
        (recur (conj diffs diff)
               diff)))))

(comment
  (parse-line "0 3 6 9 12 15")
  (parse-line "3 0 -3 -6")

  (get-diff-vec [0 3 6 9])
  (get-diff-vec [3 3 3])
  (get-diff-vec [3 0 -3 -6])
  (get-diff-vec [-3 -3 -3])

  (get-next-value [0 3 6 9 12 15])
  (get-next-value [1 3 6 10 15 21])
  (get-next-value [10 13 16 21 30 45])
  ;
  )

;; p1
(->> input-file
     read-lines
     (map parse-line)
     (map get-next-value)
     (reduce +))

(defn get-prior-value [line]
  (loop [diffs [line]
         current line]
          ;; (println diffs current)
          ;; (Thread/sleep 1000)
    (if (every? zero? current)
      (->> diffs
           (map first)
           reverse
           (reduce (fn [acc i]
                     (- i acc))))
      (let [diff (get-diff-vec current)]
        (recur (conj diffs diff)
               diff)))))

(comment
  (get-prior-value [10 13 16 21 30 45])
  (->> [10 3 0 2 0]
       reverse
       ((fn [nums]
          (reduce (fn [acc i]
                    (println acc i)
                    (- i acc))
                  nums))))
  ;
  )

;; p2
(->> input-file
     read-lines
     (map parse-line)
     (map get-prior-value)
     (reduce +))