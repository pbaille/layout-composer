(ns lcomp.pure
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as r :refer [atom]]
            [clojure.walk :as w :refer [postwalk]]
            [cljs.pprint :refer [pprint]]
            [schema.core :as s])
  (:refer-clojure :exclude [update-in]))

(defn update-in [x p f & args]
  (if-not (seq p)
    (apply f x args)
    (apply cljs.core/update-in x p f args)))


;; generic helpers -------------------------------------

#_(defn set-path [ratom path]
    (aset ratom "path" path)
    ratom)

#_(defn upd-path [ratom f & args]
    (aset ratom "path" (apply f (.-path ratom) args))
    ratom)

(comment
  (def a (atom {:a [] :b true}))
  (set-path (r/cursor a [:a]) [:b])
  (upd-path (r/cursor a [:a]) (constantly [:b])))

(defn t
  ([x] (:type (meta x)))
  ([t x] (vary-meta x assoc :type t)))

(defn t= [kw x]
  (= kw (t x)))

(defn arg1 [x & _] x)

(def t1 (comp t arg1))

(defn vcat [& xs] (vec (apply concat xs)))

(def butlastv (comp vec butlast))

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
     {:preds []
      :current :default
      :layouts {:default empty-layout}}))

(defn clayout [& [opts]]
  (t :clayout
     (merge empty-clayout opts)))

(comment
  (t= :clayout (clayout)))

;; layouts-helpers --------------------------------------

(defmulti layout-path t1)

(defmethod layout-path :layout [l] [])
(defmethod layout-path :clayout [l] [:layouts (:current l)])

(declare lpath)

(defn childs-path
  ([l] (conj (layout-path l) :childs))
  ([l path] (let [p (lpath l path)]
              (vcat p (childs-path (get-in l p))))))

(defn childs [l]
  (get-in l (childs-path l)))

(defn lpath [l p]
  (loop [current-layout l [fp & rp] p res []]
    (if fp
      (let [current-path (vcat res (conj (childs-path current-layout) fp))]
        (recur (get-in l current-path) rp current-path))
      res)))

(comment
  (assert (= (lpath (layout) []) []))
  (assert (= (lpath (add-child (add-child (clayout))) [1]) []))
  (assert (= (lpath (clayout) []) [:layouts :default])))

(defn set-path [l p]
  (let [childs-path (childs-path l)]
    (update-in (assoc-in l (conj (layout-path l) :path) p)
               childs-path
               (fn [childs]
                 (mapv #(set-path % (vcat p (:path %)))
                       childs)))))

;; API --------------------------------------------------

(defn add-child [l & [{:keys [path child idx]
                       :or {path []
                            child (layout)}}]]
  (let [childs-path (childs-path l path)
        childs (get-in l childs-path)
        idx (or idx (count childs))]
    (update-in l
               childs-path
               assoc
               idx
               (set-path child (conj path idx)))))

(defn dive [l path]
  (get-in l (lpath l path)))

(comment
  (add-child (layout))
  (pprint (add-child (clayout) {:child (clayout)}))
  (dive (add-child (add-child (clayout) {:child (clayout)}))
        [1])
  (add-child (clayout)))

(defn- rem-idx [v idx]
  (assert (and (not (neg? idx))
               (<= (count v) idx))
          (str "rem-idx idx: " idx " out of bounds " v))
  (vec (concat (subvec v 0 idx) (subvec v (inc idx)))))

(comment
  (rem-idx [1 2 3 4] 1)
  (rem-idx [1 2 3 4] 3)
  (rem-idx [1 2 3 4] -1))

(defn- reidx-childs
  ([l] (reidx-childs l []))
  ([l p]
   (update-in l
              (childs-path l p)
              (fn [childs]
                (vec
                  (map
                    (fn [idx child]
                      (let [ppath (butlastv p)]
                        (update (reidx-childs child ppath) :path (fn [cp] (conj ppath idx)))))
                    (range)
                    (remove nil? childs)))))))

(defn kill [l p]
  (let [with-killed-child (assoc-in l (lpath l p) nil)]
    (update-in with-killed-child
               (lpath l (butlastv p))
               reidx-childs)))

(comment
  (kill (add-child (dive (add-child (clayout)) [0])) [0]))

