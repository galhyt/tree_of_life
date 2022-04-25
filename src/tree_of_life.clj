(require '[clojure.string :as str])

(defn query
  [tree-state rule-code iterations-number path]
   (defn get-tree-node-value [path]
     )
   (defn get-node-new-state [path]
     )
   (defn get-tree-new-state
     ([neighbours-depth path]
      (get-tree-new-state neighbours-depth path tree-state nil))
     ([neighbours-depth path tree-new-state exclude-path]
      (if (= neighbours-depth 0)
        tree-new-state
        ((def tree-new-state (override-neighbours-states tree-state rule-code path tree-new-state))
         (let [neighbours-paths (get-neighbours-paths path exclude-path)
               i 0]
           (while [(< i (count neighbours-paths3))]
             [(def tree-new-state (recur (- neighbours-depth 1) (neighbours-paths i) tree-new-state))])
           tree-new-state)))))
   ; query function body
   (if (= iterations-number 1)
     (get-tree-node-value path)
     (let [neighbours-depth (- iterations-number 1)
           node-new-state (get-node-new-state path)]
       (if (= neighbours-depth 0)
         node-new-state
         (let [tree-new-state (get-tree-new-state neighbours-depth path)]
           (recur tree-new-state rule-code (- iterations-number 1) path))))))

(defn play-query [tree-state rule-code]
  (let [input-query read-line
        input-query-arr (str/split input-query #" ")
        iterations-number (Integer/parseInt (input-query-arr 0))
        query-path (input-query-arr 1)
        result (query tree-state rule-code iterations-number query-path)]
    ))

(defn play-the-game-of-life []
  (defn get-rule-code-struct [rule-code]
    )
  (defn get-tree-start-state-struct [tree-start-state-str]
    )
  (let [input-rule-code (read-line)
        input-tree-start-state (read-line)
        input-number-of-queries (read-line)
        rule-code (get-rule-code-struct input-rule-code)
        tree-state (get-tree-start-state-struct input-tree-start-state)
        number-of-queries (Integer/parseInt input-number-of-queries)
        i 0]
    (while [(< i number-of-queries)]
      [(play-query tree-state rule-code)])))