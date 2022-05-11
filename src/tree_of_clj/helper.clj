(ns tree-of-clj.helper
  (:require))

(def * ".")
(def x "x")
(def < "<")
(def > ">")

(defn get-value-by-path [tree path]
  (if (empty path)
    (second tree)
    (case (first path)
      > (get-value-by-path (last tree) (rest path))
      < (get-value-by-path (first tree) (rest path)))))

(defn- get-node-value [node]
  (if (seq? node)
    (get-node-value (second node))
    node))

(defn iterate-on-tree
  ([tree rule]
   (iterate-on-tree tree rule *))
  ([tree rule parent-value]
   (let [left-child (first tree)
         right-child (last tree)
         self (second tree)
         current-node-value (get-node-value self)
         new-value (rule
                 parent-value
                 (get-node-value left-child)
                 current-node-value
                 (get-node-value right-child))]
     '((iterate-on-tree left-child rule current-node-value)
      new-value
      (iterate-on-tree left-child rule current-node-value)))))
