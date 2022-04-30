(ns tree-of-life.game
  (:import (clojure.lang PersistentList)))
(require '[clojure.string :as str]
         '[clojure.edn :as edn])

(defn query [tree-state rule iterations-number path]
   ; Helper functions
   (defn get-node-value [path]
     (get tree-state path))
   (defn get-neighbours-paths [path]
     (let [parent (if (not= path []) (drop-last path) nil)
           left (conj path "<")
           right (conj path ">")]
       [parent left right]))
   (defn get-node-new-state [path]
    (let [neighbours (get-neighbours-paths path)]
      (get rule (str/join "" (map #(if (= % nil) "." (get-node-value %))
                                  (conj (take 2 neighbours) path (last neighbours)))))))
   ; Gets new state of path node and its neighbours and their neighbours and so on, according to neighbours-depth:
   ;  for neighbours-depth = 1 - path node and its neighbours, 2 - section 1 and path node neighbours neighbours and so on
   (defn get-tree-new-state [neighbours-depth path]
      (into {} (for [nodep (distinct (concat (take neighbours-depth
                                                 (iterate #(concat (for [x %] (remove nil? (get-neighbours-paths x))))
                                                          (conj (remove nil? (get-neighbours-paths path)) path)))))]
                {nodep (get-node-new-state nodep)})))
   ; query function body
   (if (= iterations-number 0)
     (get-node-value path)
     (if (= iterations-number 1)
       (get-node-new-state path)
       (let [neighbours-depth (dec iterations-number)
             tree-new-state (get-tree-new-state neighbours-depth path)]
           (recur tree-new-state rule (dec iterations-number) path)))))

(defn play-query [tree-state rule]
  (let [input-query (read-line)
        input-query-arr (str/split input-query #" ")
        iterations-number (Integer/parseInt (input-query-arr 0))
        query-path (edn/read-string (input-query-arr 1))
        result (query tree-state rule iterations-number query-path)]
      (println result)
    ))

(defn play-the-game-of-life []
  ; Helper functions
  (defn get-rule-struct [rule-code]
    (let [bin-vals (str/replace (str/replace
                                  (format "%16d" (Long/parseLong (Long/toString rule-code 2)))  "1" "x") #"[\s0]" ".")
          bin-keys (map #(str/replace (str/replace (format "%4d" (Integer/parseInt %)) "1" "x") #"[\s0]" ".")
                        (for [i (range 15 -1 -1)] (Long/toString i 2)))]
      (into {} (map (fn [k v] {k v}) bin-keys bin-vals))))
  (defn get-tree-state-struct [tree-state-str]
    (let [tree-state (edn/read-string tree-state-str)
          tree-depth (first (first (filter #(not= (type (second %)) PersistentList)
                                     (iterate #(let [index (first %) branch (second %)]
                                                 [(inc index) (first branch)]) [1 tree-state]))))]
      (into {}
        (let [paths
              (apply concat (take tree-depth
                                           (iterate #(apply concat (map (fn [path] [(conj path "<") (conj path ">")]) %)) [[]])))]
          (for [path paths]
            (if (= path [])
              {path (second tree-state)}
              {path (second (nth
                              (iterate #(let [index (first %) branch (last %) dir (path index)]
                                          (if (= index (count path))
                                            [(inc index) (if (= (type branch) PersistentList) (second branch) branch)]
                                            (if (= dir "<")
                                              [(inc index) (first branch)]
                                              (if (= dir ">")
                                                [(inc index) (last branch)]))))
                                        [0 tree-state]) (count path)))}))))))
  ; play-the-game-of-life function body
  (let [input-rule-code (read-line)
        input-tree-start-state (read-line)
        input-number-of-queries (read-line)
        rule (get-rule-struct (Integer/parseInt input-rule-code))
        tree-state (get-tree-state-struct input-tree-start-state)
        number-of-queries (Integer/parseInt input-number-of-queries)]
    (dotimes [i number-of-queries]
      (play-query tree-state rule))))

;(play-the-game-of-life)