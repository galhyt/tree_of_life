(ns tree-of-clj.helper)

(def < "<")
(def > ">")

(defn get-value-by-path [tree path]
  (if (empty? path)
    (if (seq? tree) (second tree) tree)
    (case (first path)
      ">" (get-value-by-path (last tree) (rest path))
      "<" (get-value-by-path (first tree) (rest path)))))

(defn- get-node-value [node]
  (if (= node nil)
    "."
    (if (seq? node)
      (get-node-value (second node))
      node)))

(defn iterate-on-tree
  ([tree rule]
   (iterate-on-tree tree rule "."))
  ([tree rule parent-value]
   (let [left-child (if (seq? tree) (first tree) nil)
         right-child (if (seq? tree) (last tree) nil)
         self (if (seq? tree) (second tree) tree)
         current-node-value (get-node-value self)
         new-value (rule
                 parent-value
                 (get-node-value left-child)
                 current-node-value
                 (get-node-value right-child))]
     (if (not= left-child nil)
       (list (iterate-on-tree left-child rule current-node-value)
             new-value
             (iterate-on-tree right-child rule current-node-value))
       new-value))))

(defn iterations-on-tree [tree-state rule iterations-number]
   (nth (iterate #(iterate-on-tree % rule) tree-state)
        iterations-number))