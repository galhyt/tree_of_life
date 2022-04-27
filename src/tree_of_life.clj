(require '[clojure.string :as str])

(defn query
  [tree-state rule iterations-number path]
   ; Helper functions
   (defn get-node-value [path]
     (get tree-state path))
   (defn get-node-new-state [path]
     (let [neighbours (get-neighbours-paths path)]
     (get rule (str/join "" (map #(if (= % nil) "." (get-node-value %)) (concat (take 2 neighbours) [path] (last neighbours)))))))
   (defn get-neighbours-paths [path]
     (let [parent (if (not= path []) (drop-last path) nil)
           left (concat path "<")
           right (concat path ">")]
       [parent left right]))
   ; Gets new state of path node and its neighbours and their neighbours and so on, according to neighbours-depth:
   ;  for neighbours-depth = 1 - path node and its neighbours, 2 - section 1 and path node neighbours neighbours and so on
   (defn get-tree-new-state [neighbours-depth path]
      (merge (for [nodep (distinct (concat (take neighbours-depth
                                                 (iterate #(concat (for [x %] (remove nil? (get-neighbours-paths x))))
                                                          (concat (remove nil? (get-neighbours-paths path)) path)))))]
        {nodep (get-node-new-state nodep)})))
   ; query function body
   (if (= iterations-number 0)
     (get-node-value path)
     (if (= iterations-number 1)
       (get-node-new-state path)
       (let [neighbours-depth (- iterations-number 1)
             tree-new-state (get-tree-new-state neighbours-depth path)]
           (recur tree-new-state rule (- iterations-number 1) path)))))

(defn play-query [tree-state rule]
  (let [input-query read-line
        input-query-arr (str/split input-query #" ")
        iterations-number (Integer/parseInt (input-query-arr 0))
        query-path (input-query-arr 1)
        result (query tree-state rule iterations-number query-path)]
    ))

(defn play-the-game-of-life []
  (defn get-rule-struct [rule-code]
    )
  (defn get-tree-state-struct [tree-state-str]
    )
  (let [input-rule-code (read-line)
        input-tree-start-state (read-line)
        input-number-of-queries (read-line)
        rule (get-rule-struct input-rule-code)
        tree-state (get-tree-state-struct input-tree-start-state)
        number-of-queries (Integer/parseInt input-number-of-queries)
        i 0]
    (while [(< i number-of-queries)]
      [(play-query tree-state rule)])))