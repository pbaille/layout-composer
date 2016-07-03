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

(defn vswap [v i1 i2]
  (-> v
      (assoc i1 (get v i2))
      (assoc i2 (get v i1))))

(defn idx-shift [v idx dir]
  (let [cnt (count v)]
    (condp = dir
      :left (if (zero? idx) v (vswap v idx (dec idx)))
      :right (if (= (dec cnt) idx) v (vswap v idx (inc idx))))))

(defn first-where [pred coll]
  (first (filter pred coll)))

(defn first-idx [pred coll]
  (first (first-where (comp pred second) (map vector (range) coll))))

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
     (assoc empty-layout
       :current :default
       :responses [])))

(defn clayout [& [opts]]
  (t :clayout
     (merge empty-clayout opts)))

(comment
  (t= :clayout (clayout)))

(comment
  (respond
    (clayout {:responses [{:id :res1
                           :pred (fn [{:keys [w h]}] (> w 300))
                           :layout (layout)}
                          {:id :res2
                           :pred (fn [{:keys [w h]}] (> w 500))
                           :layout (layout)}]})
    {:w 100}))

(defn lpath [p]
  (if-not (seq p)
    []
    (vec (cons :childs (interpose :childs p)))))

(comment
  (lpath [])
  (lpath [1 2]))

(defn- re-idx-childs
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

(defn brothers-path [p]
  (when (>= (count p) 1)
    (conj (lpath (butlastv p)) :childs)))

(defn oncles-path [p]
  (when (>= (count p) 2)
    (conj (lpath (butlastv (butlastv p))) :childs)))

;; layout basic action -----------------------------------------

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

(defn mv [l p dir]
  (let [bp (brothers-path p)
        op (oncles-path p)]
    (condp = dir
      :top (if op
             (insert-child (kill l p) op {:child (get-in l (lpath p))})
             l)

      :right (if bp
               (re-idx-childs
                 (update-in l bp idx-shift (last p) :right)
                 (butlastv p))
               l)

      :left (if bp
              (re-idx-childs
                (update-in l bp idx-shift (last p) :left)
                (butlastv p))
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
  (first-idx #(= id (:id %)) responses))

;actions

(defn add-response [l {:keys [response path idx]}]
  (update-in l
             (responses-path path)
             (fn [rs]
               (if idx
                 (vinsert rs idx response)
                 (conj rs response)))))

(defn kill-response [l {:keys [path idx id]}]
  (update-in l
             (responses-path path)
             #(rem-idx % (or idx (response-idx % id)))))

(defn mv-response [l {:keys [path dir idx]}]
  (update-in l
             (responses-path path)
             idx-shift
             idx
             dir))

;; constraints -----------------------------------------------

(defn add-constraint [response constraint & [idx]]
  (update response
          :constraints
          vinsert
          (or idx (count (:constraints response)))
          constraint))

(defn kill-constraint [response idx]
  (update response :constraints rem-idx idx))

(defn mv-constraint [response {:keys [dir idx]}]
  (update response :constraints idx-shift idx dir))

;;---------------------------------------------------------------
;;                      reagent-component
;;---------------------------------------------------------------

;; helpers -----------------------

(defn typed-constraint [type-vec f]
  (with-meta f {:args-types type-vec}))

(def default-constraints-map
  {:min-height
   (typed-constraint
     [:int]
     (fn [arg-map x]
       (<= x (get-in arg-map [:dimensions :h]))))
   :min-width
   (typed-constraint
     [:int]
     (fn [arg-map x]
       (<= x (get-in arg-map [:dimensions :w]))))
   :max-height
   (typed-constraint
     [:int]
     (fn [arg-map x]
       (>= x (get-in arg-map [:dimensions :h]))))
   :max-width
   (typed-constraint
     [:int]
     (fn [arg-map x]
       (>= x (get-in arg-map [:dimensions :w]))))})

(defn constraint-vec->fn [constraints-map [k & args]]
  (if-let [f (constraints-map k)]
    #(apply f % args)
    (throw (js/Error. (str "unknown constraint-key " k)))))

(defn compile-constraints
  "take a response and a constraints-map and
   return the composed function"
  [{:keys [response constraints-map]}]
  (fn [arg-map]
    (every? #(% arg-map)
            (map (fn [c]
                   (cond
                     (vector? c) (constraint-vec->fn constraints-map c)
                     (fn? c) c
                     (nil? c) identity
                     :else (throw (js/Error. "can't parse constraint " c))))
                 (:constraints response)))))

(defn styles [el]
  (js/window.getComputedStyle (r/dom-node el)))

(defn dimensions-map
  "dimensions of the current given element"
  [el]
  (let [styles (styles el)
        parse-px-str #(int (re-find #"[\d]*" %))]
    {:w (parse-px-str (.-width styles))
     :h (parse-px-str (.-height styles))}))

(defn constraints-arg-map
  "build the argument passed to all constraints"
  [this]
  {:node (r/dom-node this)
   :props (r/props this)
   :dimensions (dimensions-map this)})

(defn respond
  "respond to anything, but most probably resizing (as used in layout-comp)
   if layout doesn't need to change return nil
   else return the new layout"
  [l this]
  (when-let [rs (:responses l)]
    (let [arg-map (constraints-arg-map this)
          constraints-map (merge default-constraints-map
                                 (get-in (r/props this) [:env :constraints-map]))
          {:keys [id layout] :as fres}
          (first-where
            #((compile-constraints
                {:response %
                 :constraints-map constraints-map})
              arg-map)
            rs)]
      (when (and fres (not= id (:current l)))
        (let [rs-updated (mapv #(if (= (:current l) (:id %))
                                 (assoc %
                                   :layout
                                   (dissoc l :responses :current))
                                 %)
                               rs)]
          (assoc layout
            :responses rs-updated
            :current id))))))

;; defaults -----------------------------

(def default-layout-style
  {:display :flex
   :flex-direction "row"
   :flex-wrap "nowrap"
   :justify-content "flex-start"
   :align-items "stretch"
   :align-content "stretch"
   :order 1
   :flex-grow 1
   :flex-shrink 0
   :flex-basis "auto"
   :align-self "auto"})

(def comp-placeholder
  [:div {:style {:width :100%
                 :height :100%
                 :background :lightgrey}}])

;; main ------------------------------------

(defn layout-comp []
  (letfn [(resp [this]
            (let [layout (:layout (r/props this))
                  new-layout (respond @layout this)]
              (when new-layout
                (reset! layout new-layout))))

          (upd [this]
            (aset js/window "onresize" #(resp this))
            (resp this))]
    (r/create-class
      {:reagent-render
       (fn [{:keys [layout path env]
             :or {path []}}]
         (let [{:keys [style childs comp]} @layout]
           [:div {:data-path path
                  :style (merge default-layout-style style)}
            (if-let [xs (seq childs)]
              (for [[idx] (map vector (range) xs)]
                [layout-comp {:key (str "container-" path)
                              :layout (r/cursor layout [:childs idx])
                              :path (conj path idx)}])
              (if comp
                (let [[comp-key & comp-args] comp]
                  (into [(get-in env [:components-map comp-key]) (:state env)]
                        comp-args))
                comp-placeholder))]))
       :component-did-update upd
       :component-did-mount upd})))
