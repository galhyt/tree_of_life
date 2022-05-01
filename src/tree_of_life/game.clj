(ns tree-of-life.game
  (:import (clojure.lang PersistentList)))
(require '[clojure.string :as str]
         '[clojure.edn :as edn])

(defn get-node-value [tree-state path]
  ; Helper function
  (get tree-state path))

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


(defn query [tree-state rule iterations-number path]
   (if (= iterations-number 0)
     (get-node-value tree-state path)
     (if (= iterations-number 1)
       (get-node-new-state tree-state rule path)
       (let [neighbours-depth (dec iterations-number)
             tree-new-state (get-tree-new-state tree-state rule neighbours-depth path)]
           (recur tree-new-state rule (dec iterations-number) path)))))

(defn get-rule-struct [rule-code]
  ; Helper function
  (let [bin-vals (str/replace (str/replace
                                (format "%16d" (Long/parseLong (Long/toString rule-code 2)))  "1" "x") #"[\s0]" ".")
        bin-keys (map #(str/replace (str/replace (format "%4d" (Integer/parseInt %)) "1" "x") #"[\s0]" ".")
                      (for [i (range 15 -1 -1)] (Long/toString i 2)))]
    (into {} (map (fn [k v] {k v}) bin-keys bin-vals))))

(defn get-tree-state-struct [tree-state-str]
  ; Helper function
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

(defn game-of-life
  ; Takes raw inputs and return results
  ([rule-code tree-state-str queries-strs]
    (let [queries (for [query-str queries-strs :let [query-str-arr (str/split query-str #" ")
                                                     iterations-number (Integer/parseInt (first query-str-arr))
                                                     query-path (second query-str-arr)]]
                    {"iterations-number" iterations-number
                     "query-path" query-path
                     "result" (game-of-life rule-code tree-state-str iterations-number query-path)})]
      queries))
  ([rule-code tree-state-str iterations-number query-path-str]
   (let [rule (get-rule-struct (Integer/parseInt rule-code))
         tree-state (get-tree-state-struct tree-state-str)
         query-path (edn/read-string (query-path-str 1))]
     (query tree-state rule iterations-number query-path))))

(defn play-the-game-of-life []
  ; Entry point for solution of following link's question:
  ; https://www.hackerrank.com/challenges/the-tree-of-life/problem?h_r=internal-search
  (let [input-rule-code (read-line)
        input-tree-start-state (read-line)
        input-number-of-queries (read-line)
        number-of-queries (Integer/parseInt input-number-of-queries)]
    (dotimes [i number-of-queries :let [input-query (read-line)
                                        query-str-arr (str/split input-query #" ")
                                        iterations-number (Integer/parseInt (first query-str-arr))
                                        query-path (second query-str-arr)]]
      (game-of-life input-tree-start-state input-rule-code iterations-number query-path))))

(play-the-game-of-life)