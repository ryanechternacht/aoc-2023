(ns day10
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(def sample-file "resources/day10-sample.txt")
(def simple-file "resources/day10-sample-simple.txt")
(def sample-file-2 "resources/day10-sample-2.txt")
(def input-file "resources/day10.txt")

(defn read-lines [file]
  (->> file
       slurp
       str/split-lines))

(defn make-pipe [x y]
  {:char \-
   :neighbors #{[x (dec y)] [x (inc y)]}})

(defn make-dash [x y]
  {:char \-
   :neighbors #{[(dec x) y] [(inc x) y]}})

(defn make-L [x y]
  {:char \L
   :neighbors #{[x (dec y)] [(inc x) y]}})

(defn make-J [x y]
  {:char \J
   :neighbors #{[x (dec y)] [(dec x) y]}})

(defn make-7 [x y]
  {:char \7
   :neighbors #{[(dec x) y] [x (inc y)]}})

(defn make-F [x y]
  {:char \F
   :neighbors #{[(inc x) y] [x (inc y)]}})

(defn make-S [x y]
  {:char \S
   :neighbors #{[(dec x) y] [(inc x) y]
                [x (dec y)] [x (inc y)]}})

(defn build-map [lines]
  (reduce-kv (fn [acc y line]
               (reduce-kv (fn [acc2 x c]
                            (condp = c
                              \| (assoc acc2 [x y] (make-pipe x y))
                              \- (assoc acc2 [x y] (make-dash x y))
                              \L (assoc acc2 [x y] (make-L x y))
                              \J (assoc acc2 [x y] (make-J x y))
                              \7 (assoc acc2 [x y] (make-7 x y))
                              \F (assoc acc2 [x y] (make-F x y))
                              \S (assoc acc2 [x y] (make-S x y))
                              \. acc2))
                          acc
                          (vec line)))
             {}
             (vec lines)))

(comment
  (make-pipe 2 3)
  (make-dash 2 3)
  (make-L 2 3)
  (make-J 2 3)
  (make-7 2 3)
  (make-F 2 3)
  (make-S 2 3)

  (build-map (read-lines simple-file))
  ;
  )

(let [m (->> input-file read-lines build-map)
      starting-point (->> m
                          (filter (fn [[_ {c :char}]]
                                    (= c \S)))
                          first)
      neighbor (-> starting-point
                   second
                   :neighbors
                   vec
                   (nth 1))]
  (loop [last (first starting-point)
         current neighbor
         steps 1]
    ;; (println last current steps)
    ;; (Thread/sleep 1000)
    (let [{:keys [char neighbors]} (m current)]
      ;; (println char neighbors (-> neighbors
      ;;                             (set/difference #{last})
      ;;                             first))
      (condp = char
        nil "failure"
        \S [steps (/ steps 2)]
        (recur current
               (-> neighbors
                   (set/difference #{last})
                   first)
               (inc steps))))))
