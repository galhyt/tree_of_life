(ns tree-of-clj.core-test
  (:require
    [clojure.test :refer :all]
    [tree-of-clj.helper :refer [get-value-by-path < >
                                iterate-on-tree]]
    [clojure.string :as str]
    [clojure.edn :as edn]))

(def trees ["(x . x)"
           "((x . x) . ((x x .) x .))"])

(def rule-dict {"(x x x x)" "x"
     "(x x x .)" "."
     "(x x . x)" "."
     "(x x . .)" "x"
     "(x . x x)" "x"
     "(x . x .)" "."
     "(x . . x)" "."
     "(x . . .)" "x"
     "(. x x x)" "x"
     "(. x x .)" "."
     "(. x . x)" "."
     "(. x . .)" "x"
     "(. . x x)" "x"
     "(. . x .)" "."
     "(. . . x)" "."
     "(. . . .)" "x"})

(defn str_to_list [text]
  (def lst (edn/read-string text))
  (if (seq? lst)
    (let [root (str (second lst))
          left (str (first lst))
          right (str (last lst))]
      (list (str_to_list left) root (str_to_list right)))
    (str lst)))

(defn rule [p l c r]
  (rule-dict (str "(" (str/join " " [p l c r]) ")")))

(deftest rule-test []
  (is (= (rule "." "x" "." "x") ".")))

(deftest str_to_list-test []
  (is (= (str_to_list "(. x .)") '("." "x" "."))))

(deftest get-value-by-path-test []
  (is (= (get-value-by-path (str_to_list "(. x .)") [>]) "."))
  (is (= (get-value-by-path (str_to_list "((x x .) x .)") [<]) "x"))
  (is (= (get-value-by-path (str_to_list "((x . x) . ((x x .) x .))") [> < >]) ".")))

(deftest iterate-on-tree-test []
  (is (= (iterate-on-tree (str_to_list (trees 0)) rule) (str_to_list "(. . .)"))))
