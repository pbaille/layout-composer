(ns lcomp.pure2
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as r :refer [atom]]
            [clojure.walk :as w :refer [postwalk]]
            [cljs.pprint :refer [pprint]]
            [schema.core :as s])
  (:refer-clojure :exclude [update-in spread]))

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

;; empty layouts ---------------------------------------

(def empty-layout
  (t :layout
     {:flex-props {}
      :path []
      :childs []}))

(defn layout [& [opts]]
  (t :layout
     (merge empty-layout opts)))

(def empty-clayout
  (t :clayout
     (assoc empty-layout :responses [])))

(defn clayout [& [opts]]
  (t :clayout
     (merge empty-clayout opts)))

(comment
  (t= :clayout (clayout)))

(defn l> [l args-map]
  (if (t= :layout l)
    l
    (let [rs (:responses l)
          [_ res] (first (filter #(% args-map) rs))]
      (assoc res :responses rs))))

(defn lpath [p]
  (if-not (seq p)
    []
    (vec (cons :childs (interpose :childs p)))))

(comment
  (lpath [])
  (lpath [1 2]))

(defn re-idx-childs
  ([l]
   (update l
           :childs
           (fn [childs]
             (vec
               (map-indexed
                 (fn [idx child]
                   (re-idx-childs (assoc child :path (conj (:path l) idx))))
                 (remove nil? childs))))))
  ([l p] (update-in l (lpath p) re-idx-childs)))

(defn insert-child
  [l & [{:keys [path child idx]
         :or {path []
              child (layout)
              idx 0}}]]
  (let [childs-path (conj (lpath path) :childs)]
    (re-idx-childs
      (update-in l
                 childs-path
                 vinsert
                 idx
                 (assoc child :path (conj path idx)))
      path)))

(comment
  (insert-child (insert-child (layout) {:idx 0})
                {:child (clayout) :idx 0}))

(defn kill [l p]
  (re-idx-childs
    (update-in l (lpath p) (constantly nil))
    (butlastv p)))

(comment
  (kill (insert-child (layout) {:idx 0})
        [0]))

(defn spread [l p]
  (let [childs (get-in l (conj (lpath p) :childs))]
    (re-idx-childs
      (update-in l
                 (conj (lpath (butlastv p)) :childs)
                 (fn [cs]
                   (apply vinsert (rem-idx cs (last p)) (last p) (map #(update % :path butlastv) childs))))
      (butlastv p))))

(comment
  (let [l (insert-child (insert-child (layout)))
        insert-grand-child #(insert-child % {:path [0]})
        l (-> l
              insert-grand-child
              insert-grand-child
              insert-grand-child)]
    (pprint (spread l [0]))))

(defn wrap [l p & [wrapper]]
  (re-idx-childs
    (assoc-in l
              (lpath p)
              (insert-child (assoc (or wrapper (layout)) :path p)
                            {:child (get-in l (lpath p))}))))

(comment
  (let [l (insert-child (insert-child (layout)))
        insert-grand-child #(insert-child % {:path [0]})
        l (-> l
              insert-grand-child
              insert-grand-child
              insert-grand-child)]
    (pprint (wrap l [0] (layout)))))

