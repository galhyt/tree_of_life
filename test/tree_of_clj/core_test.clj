(ns tree-of-clj.core-test
  (:require
    [midje.sweet :refer [fact facts =>]]

    [tree-of-clj.helper :refer [get-value-by-path < > * x]])
  (:require [clojure.test :refer :all]
            [tree-of-clj.core :refer :all]))

(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))
