(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat [] a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce (fn [one other] (str one " " other)) a-seq)))

(defn my-interpose [x a-seq]
  (drop-last (reduce (fn [init elm] (conj init elm x)) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [counter _] (inc counter)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce (fn [acc e] (cons e acc)) [] a-seq))

(defn min-max-element [a-seq]
  (let [f (first a-seq)]
    (reduce (fn [[e-min e-max] e] [(min e e-min) (max e e-max)]) [f f] a-seq)))

(defn insert [sorted-seq n]
  (let [larger-than-n (partial > n)]
    (concat (take-while larger-than-n sorted-seq) [n] (drop-while larger-than-n sorted-seq))))

(defn insertion-sort [a-seq]
  (reduce (fn [sorted e] (insert sorted e)) [] a-seq))

(defn parity [a-seq]
  [:-])

(defn minus [x]
  :-)

(defn count-params [x]
  :-)

(defn my-* [x]
  :-)

(defn pred-and [x]
  (fn [x] :-))

(defn my-map [f a-seq]
  [:-])
