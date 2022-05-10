(ns tree-of-life.helper
  (:import (clojure.lang ISeq)))
(require '[clojure.string :as str]
         '[clojure.edn :as edn])

(defn get-node-value [tree-state path]
  ; Helper function
  (if (= path [])
    (str (second tree-state))
    (second (nth (iterate #(let [path (first %)
                                next-path (drop 1 path)
                                dir (first path)
                                branch ((if (= dir "<") first last) (last %))]
                             [next-path (if (= (count next-path) 0)
                                          (str (if (not (instance? ISeq branch)) branch (second branch))) branch)])
                          [path tree-state])
                    (count path)))))

(defn get-neighbours-paths [path]
      ; Helper function
      (let [parent (if (not= path []) (drop-last path) nil)
            left (conj path "<")
            right (conj path ">")]
           [parent left right]))

(defn get-node-new-state [tree-state rule path]
      ; Helper function
      (let [neighbours (get-neighbours-paths path)]
           (get rule (str/join "" (map #(if (= % nil) "." (get-node-value tree-state %))
                                       (conj (into [] (take 2 neighbours)) path (last neighbours)))))))

(defn get-tree-new-state [tree-state rule neighbours-depth path]
      ; Helper function
      ; Gets new state of path node and its neighbours and their neighbours and so on, according to neighbours-depth:
      ;  for neighbours-depth = 1 - path node and its neighbours, 2 - section 1 and path node neighbours neighbours and so on
      (into {} (for [nodep (distinct (concat (take neighbours-depth
                                                   (iterate #(concat (for [x %] (remove nil? (get-neighbours-paths x))))
                                                            (conj (remove nil? (get-neighbours-paths path)) path)))))]
                    {nodep (get-node-new-state tree-state rule nodep)})))

(defn get-rule-struct [rule-code]
      ; Helper function
      (let [bin-vals (str/replace (str/replace
                                    (format "%16d" (Long/parseLong (Long/toString rule-code 2)))  "1" "x") #"[\s0]" ".")
            bin-keys (map #(str/replace (str/replace (format "%4d" (Integer/parseInt %)) "1" "x") #"[\s0]" ".")
                          (for [i (range 15 -1 -1)] (Long/toString i 2)))]
           (into {} (map (fn [k v] {k v}) bin-keys bin-vals))))
