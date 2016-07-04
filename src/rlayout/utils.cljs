(ns rlayout.utils
  (:refer-clojure :exclude [update-in]))

(defn update-in [x p f & args]
  (if-not (seq p)
    (apply f x args)
    (apply cljs.core/update-in x p f args)))

(defn t
  ([x] (:type (meta x)))
  ([t x] (vary-meta x assoc :type t)))

(defn t= [kw x]
  (= kw (t x)))

(defn arg1 [x & _] x)

(def t1 (comp t arg1))

(defn vcat [& xs] (vec (apply concat xs)))

(def butlastv (comp vec butlast))

(defn vinsert [v idx & vls]
  (vec (concat (subvec v 0 idx) vls (subvec v idx))))

(comment
  (vinsert [1 2] 1 8)
  (vinsert [1 2] 1 8 9 0))

(defn- rem-idx [v idx]
  (vec (concat (subvec v 0 idx) (subvec v (inc idx)))))

(defn vswap [v i1 i2]
  (-> v
      (assoc i1 (get v i2))
      (assoc i2 (get v i1))))

(defn idx-shift [v idx dir]
  (let [cnt (count v)]
    (condp = dir
      :left (if (zero? idx)
              (conj (vec (next v)) (first v))
              (vswap v idx (dec idx)))
      :right (if (= (dec cnt) idx)
               (vec (cons (last v) (butlast v)))
               (vswap v idx (inc idx))))))

(defn mod-shift [modulo idx dir]
  (condp = dir
    :left (mod (dec idx) modulo)
    :right (mod (inc idx) modulo)))

(defn first-where [pred coll]
  (first (filter pred coll)))

(defn first-idx [pred coll]
  (first (first-where (comp pred second) (map vector (range) coll))))

(defn indexed [coll]
  (map vector (range) coll))
