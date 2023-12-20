(ns day15
  (:require [clojure.string :as str]))

(def sample-file "resources/day15-sample.txt")
(def input-file "resources/day15.txt")

(defn read-file [file]
  (-> file
      slurp
      (str/split #",")))

(defn hash-char [acc c]
  (-> c
      int
      (+ acc)
      (* 17)
      (mod 256)))

(defn hash-algo [str]
  (reduce hash-char 0 str))

(comment
  (read-file sample-file)

  (hash-char 0 \H)
  (hash-char 200 \A)
  (hash-char 153 \S)
  (hash-char 172 \H)

  (hash-algo "HASH")
  ;
  )

;; p1
(->> input-file
     read-file
     (map hash-algo)
     (reduce +))

(defn make-boxes []
  (->> (range 256)
       (map (fn [x] {x []}))
       (into {})))

(defn parse-command [s]
  (let [box (re-find #"\w+" s)]
    {:box box
     :box-num (hash-algo box)
     :cmd (.charAt (re-find #"[-=]" s) 0)
     :lens (when-let [l (re-find #"\d" s)]
             (parse-long l))}))

(defn run-command [boxes {:keys [box box-num cmd lens]}]
  (if (= cmd \-)
    (update boxes box-num #(vec (remove (fn [[b]] (= b box)) %)))
    ;; cmd \=
    (let [lenses (boxes box-num)]
      (if-let [item (->> lenses
                         (filter (fn [[b _]]
                                   (= b box)))
                         first)]
        (update boxes box-num #(replace {item [box lens]} %))
        (update boxes box-num conj [box lens])))))

(defn run-commands [cmds]
  (reduce run-command
          (make-boxes)
          cmds))

(defn sum-focus-power [lenses]
  (reduce (fn [acc [box items]]
            (let [power (reduce-kv (fn [acc2 i [_ lens]]
                                     (+ acc2
                                        (* (inc box) (inc i) lens)))
                                   0
                                   items)]
              (+ acc
                 power)))
          0
          lenses))

(comment
  (hash-algo "rn")
  (make-boxes)

  (parse-command "rn=1")
  (parse-command "qp-")
  (parse-command "hbpmm-")
  (parse-command "kphvsd=2")
  
  ;; remove item failure
  (run-command {0 [["aa" 3]]} {:box "rn" :box-num 0 :cmd \-})

  ;; remove item success
  (run-command {0 [["rn" 3]]} {:box "rn" :box-num 0 :cmd \-})

  ;; swap item
  (run-command {0 [["aa" 3] ["rn" 1] ["bb 8"]]} {:box "rn" :box-num 0 :cmd \= :lens 3})

  ;; add item
  (run-command {0 [["ab" 3]]} {:box "rn" :box-num 0 :cmd \= :lens 3})

  (sum-focus-power {0 [["rn" 1] ["cm" 2]]
                    3 [["ot" 7] ["ab" 5] ["pc" 6]]
                    4 []
                    5 []})
  ;
  )

(->> input-file
     read-file
     (map parse-command)
     run-commands
     (filter (fn [[_ l]] (seq l)))
     sum-focus-power)