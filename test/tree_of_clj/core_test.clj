(ns tree-of-clj.core-test
  (:require
    [clojure.test :refer :all]
    [tree-of-clj.helper :refer [get-value-by-path < > o x
                                iterate-on-tree]]))

(def trees ['(x o x)
           '((x o x) o ((x x o) x o))])

(def rule-dict {'(x x x x) 'x
     '(x x x o) 'o
     '(x x o x) 'o
     '(x x o o) 'x
     '(x o x x) 'x
     '(x o x o) 'o
     '(x o o x) 'o
     '(x o o o) 'x
     '(o x x x) 'x
     '(o x x o) 'o
     '(o x o x) 'o
     '(o x o o) 'x
     '(o o x x) 'x
     '(o o x o) 'o
     '(o o o x) 'o
     '(o o o o) 'x})

(deftest get-value-by-path-test []
    (is (= (get-value-by-path '(o x o) [>]) 'o))
    (is (= (get-value-by-path '((x x o) x o) [<]) 'x))
    (is (= (get-value-by-path '((x o x) o ((x x o) x o)) [> < >]) 'o)))

(defn rule [p l c r]
  (rule-dict '(p l c r)))

(deftest iterate-on-tree-test []
    (is (= (iterate-on-tree (trees 0) rule) '(o x x))))
