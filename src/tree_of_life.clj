(require '[clojure.string :as str])

(defn query
  [tree-state rule iterations-number path]
   ; Helper functions
   (defn get-node-value [path]
     )
   (defn get-node-new-state [path]
     )
   (defn override-neighbours-states [path tree-new-state]
     )
   (defn get-neighbours-paths [path exclude-path]
     )
   ; Gets new state of path node and its neighbours and their neighbours and so on, according to neighbours-depth:
   ;  for neighbours-depth = 1 - path node and its neighbours, 2 - section 1 and path node neighbours neighbours and so on
   (defn get-tree-new-state
     ([neighbours-depth path]
      (get-tree-new-state neighbours-depth path {} nil))
     ([neighbours-depth path tree-new-state exclude-path]
      (if (= neighbours-depth 0)
        tree-new-state
        ((def tree-new-state (override-neighbours-states path tree-new-state))
         (if (= neighbours-depth 1)
           tree-new-state
           (let [neighbours-paths (get-neighbours-paths path exclude-path)
                 i 0]
             (while [(< i (count neighbours-paths))]
               [(def tree-new-state (recur (- neighbours-depth 1) (neighbours-paths i) tree-new-state path))])
             tree-new-state)))))
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