(require 'clojure.string)

(def k (Integer/parseInt (first  *command-line-args*)))
(def n (Integer/parseInt (second *command-line-args*)))

(defn next_seed [seed]
  (+
    (mod (* seed n) (int (Math/floor (Math/pow n k))))
    (loop [seed seed acc 0]
      (if (zero? seed) 
          (mod acc n)
          (recur (quot seed n) (+ acc (mod seed n)))))))

(defn cycles []
  (reduce #(assoc %1 %2 (next_seed %2)) {} (range (Math/pow n k))))

(defn unspool [m orig]
  (loop [k orig acc '()]
    (if (or
          (and (= k orig) (not (empty? acc)))
          (not (contains? m k)))
        (reverse acc)
        (recur (get m k) (conj acc k)))))

(defn unspool-all [m]
  (loop [m m acc '()]
    (if (empty? m)
        acc
        (let [spool (unspool m (first (keys m)))]
          (recur (apply dissoc m spool) (conj acc spool))))))

(doseq
  [spool (unspool-all (cycles))]
  (println
    (clojure.string/join
      " "
      (map #(mod % n)
        (conj spool (last spool))))))
