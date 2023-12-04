(ns day03
  (:require [clojure.string :as str]))

(def sample-file "resources/day03-sample.txt")
(def input-file "resources/day03.txt")

(defn read-lines [file]
  (->> file
       slurp
       str/split-lines))

(defn get-numbers-from-line [line-num line]
  (let [matcher (re-matcher #"\d+" line)]
    (loop [m matcher
           acc []]
      (if-let [next-match (re-find matcher)]
        (let [start (.start m)
              spaces (map (fn [i] [(+ i start) line-num]) (range (count next-match)))]
          (recur m (conj acc {:type :number
                              :num (parse-long next-match)
                              :spaces (into #{} spaces)})))
        acc))))

(defn get-parts-from-line [line-num line]
  (let [matcher (re-matcher #"[^\.\d]" line)]
    (loop [m matcher
           acc []]
      (if-let [next-match (re-find matcher)]
        (recur m (conj acc {:type :part
                            :symbol next-match
                            :space [(.start m) line-num]}))
        acc))))

(defn map-number-spots-to-positions [lines]
  (let [all-nums (->> lines
                      (map-indexed get-numbers-from-line)
                      flatten)]
    (reduce (fn [acc {:keys [spaces] :as num}]
              (reduce (fn [acc2 space]
                        (assoc acc2 space num))
                      acc
                      spaces))
            {}
            all-nums)))

(defn map-part-adjancies [lines]
  (let [all-spaces (->> lines
                        (map-indexed get-parts-from-line)
                        flatten)]
    (reduce (fn [acc {[x y] :space}]
              (into acc (for [x-adjust (range -1 2)
                              y-adjust (range -1 2)]
                          [(+ x x-adjust) (+ y y-adjust)])))
            #{}
            all-spaces)))

(comment
  (get-numbers-from-line 0 "467..114..")
  (get-parts-from-line 0 "467..114..")
  (get-numbers-from-line 0 "617*......")
  (get-parts-from-line 0 "617*......")
  (get-parts-from-line 0 "617*$..#..")

  (map-number-spots-to-positions (read-lines sample-file))
  (map-part-adjancies (read-lines sample-file))
  ;
  )
;; p1
(let [lines (read-lines input-file)
      part-nums? (map-number-spots-to-positions lines)
      part-adjancies (map-part-adjancies lines)]
  (->> part-adjancies
       (map #(get part-nums? %))
       (filter identity)
       distinct
       (map :num)
       (reduce +)))

(defn  add-adjancies-to-part [{[x y] :space :as part}]
  (let [adjancies
        (for [x-adjust (range -1 2)
              y-adjust (range -1 2)]
          [(+ x x-adjust) (+ y y-adjust)])]
    (assoc part :adjancies adjancies)))

(defn add-adjancies-to-parts [lines]
  (->> lines
       (map-indexed get-parts-from-line)
       flatten
       (map add-adjancies-to-part)))

(defn add-adjancent-parts-nums [part-nums? {adjancies :adjancies :as part}]
  (let [adjancent-parts (reduce (fn [acc adj]
                                  (if-let [p (get part-nums? adj)]
                                    (conj acc p)
                                    acc))
                                []
                                adjancies)]
    (assoc part :adjancent-parts (distinct adjancent-parts))))

(comment
  (add-adjancies-to-part {:type :part, :symbol "*", :space [3 1]})
  (add-adjancies-to-parts (read-lines sample-file))
  (add-adjancent-parts-nums (map-number-spots-to-positions (read-lines sample-file))
                            {:type :part, :symbol "*", :space [3 1], :adjancies '([2 0] [2 1] [2 2] [3 0] [3 1] [3 2] [4 0] [4 1] [4 2])})
  ;
  )

;; p2
(let [lines (read-lines input-file)
      part-nums? (map-number-spots-to-positions lines)]
  (->> lines
       add-adjancies-to-parts
       (map #(add-adjancent-parts-nums part-nums? %))
       (filter #(> (count (:adjancent-parts %)) 1))
       (map (fn [{:keys [adjancent-parts]}]
              (reduce * (map :num adjancent-parts))))
       (reduce +)))