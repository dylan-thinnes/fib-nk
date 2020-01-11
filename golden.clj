(require 'clojure.string)

(defn next_seed [seed k n]
  (+
    (mod (* seed n) (int (Math/floor (Math/pow n k))))
    (loop [seed seed acc 0]
      (if (zero? seed) 
          (mod acc n)
          (recur (quot seed n) (+ acc (mod seed n)))))))

(defn cycles [k n]
  (reduce #(assoc %1 %2 (next_seed %2 k n)) {} (range (Math/pow n k))))

(defn unspool [m orig]
  (loop [k orig acc []]
    (if (or
          (and (= k orig) (not (empty? acc)))
          (not (contains? m k)))
        acc
        (recur (get m k) (conj acc k)))))

(defn unspool-all [m]
  (loop [m m acc []]
    (if (empty? m)
        acc
        (let [spool (unspool m (first (keys m)))]
          (recur (apply dissoc m spool) (conj acc spool))))))

(def k (Integer/parseInt (first  *command-line-args*)))
(def n (Integer/parseInt (second *command-line-args*)))

(doseq
  [spool (unspool-all (cycles k n))]
  (println
    (clojure.string/join
      " "
      (mapv #(mod % n)
        (apply conj spool (take (- k 1) spool))))))
