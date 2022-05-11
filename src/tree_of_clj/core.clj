(ns tree-of-clj.core
  (:gen-class)
  (require '[clojure.string :as str]
           '[clojure.edn :as edn]
           '[tree-of-life.helper :refer [get-node-value get-node-new-state get-tree-new-state
                                         get-rule-struct]]))
(defn query [tree-state rule iterations-number path]
  (if (= iterations-number 0)
    (get-value-by-path tree-state path)
    (if (= iterations-number 1)
      (get-node-new-state tree-state rule path)
      (let [neighbours-depth (dec iterations-number)
            tree-new-state (get-tree-new-state tree-state rule neighbours-depth path)]
        (recur tree-new-state rule (dec iterations-number) path)))))

(defn game-of-life
  ; Takes raw inputs and return results
  ([tree-state-str rule-code queries-strs]
   (let [queries (for [query-str queries-strs :let [query-str-arr (str/split query-str #" ")
                                                    iterations-number (Integer/parseInt (first query-str-arr))
                                                    query-path (second query-str-arr)]]
                   {"iterations-number" iterations-number
                    "query-path" query-path
                    "result" (game-of-life rule-code tree-state-str iterations-number query-path)})]
     queries))
  ([tree-state-str rule-code iterations-number query-path-str]
   (let [rule (get-rule-struct (Integer/parseInt rule-code))
         tree-state (edn/read-string tree-state-str)
         query-path (into [] (map str (edn/read-string query-path-str)))]
     (query tree-state rule iterations-number query-path))))

(defn play-the-game-of-life []
  ; Entry point for solution of following link's question:
  ; https://www.hackerrank.com/challenges/the-tree-of-life/problem?h_r=internal-search
  (let [input-rule-code (read-line)
        input-tree-start-state (read-line)
        input-number-of-queries (read-line)
        number-of-queries (Integer/parseInt input-number-of-queries)]
    (dotimes [i number-of-queries]
      (let [input-query (read-line)
            query-str-arr (str/split input-query #" ")
            iterations-number (Integer/parseInt (first query-str-arr))
            query-path (second query-str-arr)]
        (game-of-life input-tree-start-state input-rule-code iterations-number query-path)))))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
