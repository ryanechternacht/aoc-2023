(ns day08
  (:require [clojure.string :as str]))

(def sample-file "resources/day08-sample.txt")
(def sample-file-2 "resources/day08-sample-2.txt")
(def sample-file-3 "resources/day08-sample-3.txt")
(def input-file "resources/day08.txt")

(defn read-data [file]
  (let [[rls _ & others](->> file
             slurp
             str/split-lines)
        nodes (reduce (fn [acc line]
                        (let [[root left right] (re-seq #"\w+" line)]
                          (-> acc
                              (assoc [root \L] left)
                              (assoc [root \R] right))))
                      {}
                      others)]
    {:rls rls
     :nodes nodes}))

;; p1
(let [{:keys [nodes rls]} (read-data input-file)
      rls (cycle rls)]
  (loop [node "AAA"
         steps 0]
    (if (= node "ZZZ")
      steps
      (recur (nodes [node (nth rls steps)]) (inc steps)))))

(defn third [[_ _ t]]
  t)

(defn third-is-z [coll]
  (= \Z (third coll)))

(defn cycle-nth [coll index]
  (let [i (mod index (count coll))]
    (nth coll i)))

(comment 
  (third [1 2 3])
  (third [1 2])
  (third "ABCD")
  (third-is-z "ABZ")
  (third-is-z "ABD")

  (cycle-nth "abc123" 3)
  (cycle-nth "abc123" 6)
  (cycle-nth "abc123" 9)
  ;
  )

;; p2
;; (let [{:keys [nodes rls]} (read-data input-file)
;;       third-is-a (->> nodes
;;                       (map first)
;;                       (map first)
;;                       distinct
;;                       (filter #(= \A (third %))))]
;;   (loop [current-nodes ["ZZZ" "NNZ" "PMZ" "DKZ" "DBZ" "NJZ"]
;;          steps 10818234074807]
;;     (Thread/sleep 1000)
;;     (println steps current-nodes)
;;     ;; (when (zero? (mod steps 100000))
;;     ;;   (println steps current-nodes))
;;     (if (every? third-is-z current-nodes)
;;       steps
;;       (let [rl (cycle-nth rls steps)]
;;         (recur (map #(nodes [% rl]) current-nodes) (inc steps))))))

(let [{:keys [nodes rls]} (read-data input-file)]
  (loop [node "VMD"
         steps 22200]
    (let [rl (cycle-nth rls steps)]
      ;; (Thread/sleep 100)
      ;; (println rl)
      (if (third-is-z node)
        [steps node rl]
        (recur (nodes [node rl]) (inc steps))))))

;; AAA -> 17141 -> ZZZ -> 34282 -> ZZZ
;; AAA 17141 offset, 17141 cycle length

;; XQA -> 16579 -> NNZ -> 33158 -> NNZ
;; 16579 offset, 16579 cycle length

;; SKA -> 18827 -> PMZ -> 37654 -> PMZ
;; 18827 offset and cycle

;; NQA -> 12083 -> DKZ -> 24166 -> DKZ
;; 12083 offset and cycle

;; LJA -> 13207 -> DBZ -> 26414 -> DBZ
;; 13207 offset and cycle

;; NVA -> 22199 -> NJZ -> 44398 -> NJZ
;; 22199 offset and cycle length

;; 10818234074807