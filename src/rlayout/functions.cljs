(ns rlayout.functions
  (:require [cljs.pprint :refer [pprint]]
            [rlayout.utils :as u :refer [t]])
  (:refer-clojure :exclude [spread]))

;; empty layouts ---------------------------------------

(def empty-layout
  (t :layout
     {:current :default
      :responses []
      :childs []}))

(defn layout [& [opts]]
  (t :layout
     (merge empty-layout opts)))

(defn lpath [p]
  (if-not (seq p)
    []
    (vec (cons :childs (interpose :childs p)))))

(comment
  (lpath [])
  (lpath [1 2]))

(defn childs-path [p]
  (conj (lpath p) :childs))

(defn brothers-path [p]
  (when (>= (count p) 1)
    (conj (lpath (u/butlastv p)) :childs)))

(defn oncles-path [p]
  (when (>= (count p) 2)
    (conj (lpath (u/butlastv (u/butlastv p))) :childs)))

;; rlayout basic action -----------------------------------------

(defn insert-child
  [l & [{:keys [path child idx]
         :or {path []
              child (layout)
              idx 0}}]]
  (u/update-in l
             (childs-path path)
             u/vinsert
             idx
             child))

(comment
  (insert-child (insert-child (layout) {:idx 0})
                {:child (layout) :idx 0}))

(defn kill [l p]
  (u/update-in l (brothers-path p) u/rem-idx (last p)))

(comment
  (kill (insert-child (layout) {:idx 0})
        [0]))

(defn spread [l p]
  (let [childs (get-in l (childs-path p))]
    (u/update-in l
               (brothers-path p)
               (fn [cs]
                 (apply u/vinsert (u/rem-idx cs (last p)) (last p) childs)))))

(comment
  (let [l (insert-child (insert-child (layout)))
        insert-grand-child #(insert-child % {:path [0]})
        l (-> l
              insert-grand-child
              insert-grand-child
              insert-grand-child)]
    (pprint l)
    (pprint (spread l [0]))))

(defn wrap [l p & [wrapper]]
  (assoc-in l
            (lpath p)
            (insert-child (or wrapper (layout))
                          {:child (get-in l (lpath p))})))

(comment
  (let [l (insert-child (insert-child (layout)))
        insert-grand-child #(insert-child % {:path [0]})
        l (-> l
              insert-grand-child
              insert-grand-child
              insert-grand-child)]
    (pprint l)
    (pprint (wrap l [0] (layout)))))

(defn mv [l p dir]
  (let [bp (brothers-path p)
        op (oncles-path p)]
    (condp = dir
      :top (if op
             (insert-child (kill l p) op {:child (get-in l (lpath p))})
             l)

      :right (if bp
               (update-in l bp u/idx-shift (last p) :right)
               l)

      :left (if bp
              (update-in l bp u/idx-shift (last p) :left)
              l))))

(comment
  (let [l (insert-child (insert-child (layout)))
        insert-grand-child #(insert-child % {:path [0]})
        l (-> l
              insert-grand-child
              insert-grand-child
              insert-grand-child)]
    #_(pprint (mv l [0 1] :left))
    #_(pprint (mv l [0 1] :right))
    (pprint l)
    (pprint (mv l [0 1] :top))
    (pprint (mv (mv l [0 1] :top) [1] :right))))

;; responses -------------------------------------------------

;paths

(defn responses-path [p]
  (conj (lpath p) :responses))

(defn response-idx [responses id]
  (u/first-idx #(= id (:id %)) responses))

;actions

(defn add-response [l {:keys [response path idx]}]
  (update-in l
             (responses-path path)
             (fn [rs]
               (if idx
                 (u/vinsert rs idx response)
                 (conj rs response)))))

(defn kill-response [l {:keys [path idx id]}]
  (update-in l
             (responses-path path)
             #(u/rem-idx % (or idx (response-idx % id)))))

(defn mv-response [l {:keys [path dir idx]}]
  (update-in l
             (responses-path path)
             u/idx-shift
             idx
             dir))

;; constraints -----------------------------------------------

(defn add-constraint [response constraint & [idx]]
  (update response
          :constraints
          u/vinsert
          (or idx (count (:constraints response)))
          constraint))

(defn kill-constraint [response idx]
  (update response :constraints u/rem-idx idx))

(defn mv-constraint [response {:keys [dir idx]}]
  (update response :constraints u/idx-shift idx dir))

;;---------------------------------------------------------------
;;                      reagent-component
;;---------------------------------------------------------------


