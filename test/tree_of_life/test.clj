(require '[tree-of-life.game :as tol]
         '[clojure.test :refer [deftest testing is are run-tests]])

(deftest tree-of-life-test []
         (is (= 5 (+ 1 4))))

(run-tests)