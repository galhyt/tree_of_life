(require '[clojure.string :as str])

(defn sum_digits [x]
      (apply + (map #(Integer/parseInt %) (str/split (str x) #"")))
      )

(defn super_digit [x]
  (if (< x 10) x
      (recur (sum_digits x)))
  )

(defn sp_of_concatenated [n k]
      (super_digit (Integer/parseInt (nth (iterate #(str n %) (str n)) (- k 1))))
      )

(let [line (read-line)
      line_vector (map #(Integer/parseInt %) (str/split line #" "))
      n (nth line_vector 0)
      k (nth line_vector 1)]
      (sp_of_concatenated n k)
     )
