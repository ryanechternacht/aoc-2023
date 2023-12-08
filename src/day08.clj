(ns day08
  (:require [clojure.string :as str]))

(def sample-file "resources/day08-sample.txt")
(def sample-file-2 "resources/day08-sample-2.txt")
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

(let [{:keys [nodes rls]} (read-data input-file)
      rls (cycle rls)]
  (loop [node "AAA"
         steps 0]
    (if (= node "ZZZ")
      steps
      (recur (nodes [node (nth rls steps)]) (inc steps)))))
