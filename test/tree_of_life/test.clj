(require '[tree-of-life.game :as tol]
         '[clojure.test :refer [deftest testing is are run-tests use-fixtures]]
         '[tree-of-life.helper :refer [get-node-value get-node-new-state get-tree-new-state
                                       get-rule-struct get-tree-state-struct]])

(deftest get-tree-state-struct-test []
         (let [tree-state-tests {"()" {[] nil}
                                "(x)" {[] "x"}
                                "(. x .)" {[] "x" ["<"] "." [">"] "."}}]
           (doseq [[input result] tree-state-tests]
             (is (= (get-tree-state-struct input) result)))))

(deftest tree-of-life-test []
         )

(defn tree-state [test-function]
  )

(run-tests)