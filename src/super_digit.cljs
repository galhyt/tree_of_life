(require '[clojure.string :as str])

(defn sum_digits [x]
      (apply + (map #(Integer/parseInt %) (str/split x #"")))
      )

(defn super_digit [x]
  (if (= (count x) 1) x
      (recur (str (sum_digits x))))
  )

(defn sp_of_concatenated [n k]
      (Integer/parseInt (super_digit (nth (iterate #(str n %) (str n)) (- k 1))))
      )

(let [line (read-line)
      line_vector (map #(Integer/parseInt %) (str/split line #" "))
      n (nth line_vector 0)
      k (nth line_vector 1)]
      (sp_of_concatenated n k)
     )
