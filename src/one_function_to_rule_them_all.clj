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
  (let [toggle (fn [a-set e] (if (contains? a-set e) (disj a-set e) (conj a-set e)))]
    (reduce toggle #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))

(defn count-params [& x]
  (count x))

(defn my-*
  ([] 1)
  ([& numbers] (reduce * numbers)))

(defn pred-and [& preds]
  (fn [x] (reduce (fn [init p] (and init (p x))) true preds)))

(defn my-map [f & seqs]
  (cond
    (= 1 (count seqs))
      ; One sequence with elemnts is easy
      (reduce (fn [r x] (concat r [(f x)])) '() (first seqs))
    (true? (reduce (fn [acc e] (and acc (empty? e))) true seqs))
      ; A list of sequences with empty sequences is easy
      '()
    :else
      ; Grab all the first elements and apply the function to them, grab the rest of the subsequeces, and recurse
      (concat [(apply f (my-map first seqs))] (apply my-map f (my-map rest seqs)))))
