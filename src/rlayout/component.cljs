(ns rlayout.component
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as r :refer [atom]]
            [cljs.pprint :refer [pprint]]
            [rlayout.utils :as u]
            [rlayout.functions :as f])
  (:refer-clojure :exclude [spread]))

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
  "respond to anything, but most probably resizing (as used in rlayout-comp)
   if rlayout doesn't need to change return nil
   else return the new rlayout"
  [l this]
  (when-let [rs (:responses l)]
    (let [arg-map (constraints-arg-map this)
          constraints-map (merge default-constraints-map
                                 (get-in (r/props this) [:env :constraints-map]))
          {:keys [id layout] :as fres}
          (u/first-where
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

(def default-placeholder-component
  [:div {:style {:width :100%
                 :height :100%
                 :background :lightgrey}}])

;; main ------------------------------------

(defn layout-comp []
  (letfn [(resp [this]
            (let [layout (:layout (r/props this))]
              (when-let [new-layout (respond @layout this)]
                (reset! layout new-layout))))

          (upd [this]
            (aset js/window "onresize" #(resp this))
            (resp this))]
    (r/create-class
      {:component-did-update upd
       :component-did-mount upd
       :reagent-render
       (fn [{:keys [layout path env]
             :or {path []}}]
         (let [{:keys [style childs comp]} @layout]
           [:div {:data-path path
                  :style (merge default-layout-style style)}
            (if-let [xs (seq childs)]
              (for [[idx] (u/indexed xs)]
                [layout-comp {:key (str "container-" path)
                              :layout (r/cursor layout [:childs idx])
                              :path (conj path idx)}])
              (if comp
                (let [[comp-key & comp-args] comp]
                  (into [(get-in env [:components-map comp-key]) (:state env)]
                        comp-args))
                (:placeholder-component env default-placeholder-component)))]))})))
