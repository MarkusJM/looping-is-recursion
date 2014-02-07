(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base n] (if (== n 0) acc
                                  (recur (* acc base) base (dec n))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [last a-seq] (if (empty? a-seq) last
                                  (recur (first a-seq) (rest a-seq))))]
    (helper (first a-seq) (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond 
    (and (empty? seq1) (empty? seq2)) true
    (or (and (empty? seq1) (not (empty? seq2))) (and (empty? seq2) (not (empty? seq1)))) false
    (== (first seq1) (first seq2)) (seq= (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
  (loop [indx 0
         n (count a-seq)]
    (cond
      (zero? n) nil
      (pred (get a-seq indx)) indx
      :else (recur (inc indx) (dec n)))))

(defn avg [a-seq]
  (loop [sum 0
         n (count a-seq)
         indx 0]
    (cond
      (zero? n) (if (> indx 0) (/ sum indx) nil)
      :else (recur (+ sum (get a-seq indx)) (dec n) (inc indx)))))

(defn toggle [a-set elem]
  (cond
    (contains? a-set elem) (disj a-set elem)
    :else (conj a-set elem)))

(defn parity [a-seq]
  (loop [n (count a-seq)
         odd-set #{}
         indx 0]
    (cond
      (zero? n) odd-set
      :else (recur (dec n) (toggle odd-set (get a-seq indx)) (inc indx)))))

(defn fast-fibo [n]
  (loop [val-n-2 0
         val-n-1 1
         indx 2]
    (cond
      (== n 0) 0
      (== n 1) 1
      (>= indx n) (+ val-n-2 val-n-1)
      :else (recur val-n-1 (+ val-n-2 val-n-1) (inc indx)))))

(defn cut-at-repetition [a-seq]
  (loop [items #{}
         indx 0
         n (count a-seq)
         result []]
    (cond
      (== n 0) result
      (contains? items (get a-seq indx)) result
      :else (recur (conj items (get a-seq indx)) (inc indx) (dec n) (conj result  (get a-seq indx)))))) 
      

