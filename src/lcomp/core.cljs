(ns lcomp.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as r :refer [atom]]
            [clojure.walk :as w :refer [postwalk]]
            [schema.core :as s]))

(enable-console-print!)

;; helpers ----------------------------------------------------------

(defn $ [s] (array-seq (js/document.querySelectorAll s)))
(defn $1 [s] (first ($ s)))
(defn tval [e] (.. e -target -value))

;; defaults ----------------------------------------------------------

(def default-pad "10px")
(def step 0.1)
(def border-size 6)

(def default-flex-props
  {:flex-direction "row"
   :flex-wrap "nowrap"
   :justify-content "flex-start"
   :align-items "stretch"
   :align-content "stretch"
   :order 1
   :flex-grow 1
   :flex-shrink 0
   :flex-basis "auto"
   :align-self "auto"})

(def empty-layout {:flex-props {} :path [] :childs []})

;; flex-props specs --------------------------------------------------

(def flex-parent-opts
  {:flex-direction #{:row :column}
   :flex-wrap #{:wrap :nowrap :wrap-reverse}
   :justify-content #{:flex-start
                      :flex-end
                      :center
                      :space-between
                      :space-around}
   :align-items #{:flex-start
                  :flex-end
                  :center
                  :baseline
                  :stretch}
   :align-content #{:flex-start
                    :flex-end
                    :center
                    :stretch
                    :space-between
                    :space-around}})

(def flex-child-opts
  {:order :int
   :flex-grow :float
   :flex-shrink :float
   :flex-basis #{:auto
                 {:val :float
                  :unit [:% :px :em :rem :vh :vw :vmin :vmax :ex :cm :mm :in :pt :pc]}}
   :align-self #{:auto
                 :flex-start
                 :flex-end
                 :center
                 :baseline
                 :stretch}})

(def flex-opts
  (merge flex-child-opts
         flex-parent-opts))

;; sub components ------------------------------------------------------

(defn select-coll [state k]
  (let [c (r/cursor state [:flex-props k])]
    (fn []
      [:div
       {:style {:display :flex
                :flex-flow "row nowrap"
                :justify-content "space-between"
                :padding :3px}}
       [:span {:style {:padding "0 10px"}}
        (name k)]
       (into [:select {:style {:width :100px}
                       :value (or @c (get default-flex-props k))
                       :on-change (fn [e] (reset! c (tval e)))}]
             (doall
               (for [o (get flex-opts k)]
                 [:option {:value (name o)
                           :key o}
                  (name o)])))])))

(defn num-input [state k]
  (let [c (r/cursor state [:flex-props k])]
    [:div {:style {:display :flex
                   :flex-flow "row nowrap"
                   :justify-content "space-between"
                   :padding :3px}}
     [:span {:style {:padding "0 10px"}}
      (name k)]
     [:input {:style {:width :100px}
              :on-change (fn [e] (reset! c (tval e)))
              :type "number"
              :value (or @c (get default-flex-props k))}]]))

(defn text-input [state k]
  (let [c (r/cursor state [:flex-props k])]
    [:div {:style {:display :flex
                   :flex-flow "row nowrap"
                   :justify-content "space-between"
                   :padding :3px}}
     [:span {:style {:padding "0 10px"}}
      (name k)]
     [:input {:style {:width :100px}
              :on-change (fn [e] (reset! c (tval e)))
              :type "text"
              :value (or @c (get default-flex-props k))}]]))

(defn action [n click-handler]
  [:div {:style {:padding :5px
                 :margin "10px 4px"
                 :background :lightgrey
                 :border-radius :4px}
         :on-click click-handler}
   n])

;; impl helpers ------------------------------------------------------

(defn- re-idx [x]
  (assoc x :childs
           (vec
             (map-indexed

               (fn [idx child]
                 (re-idx
                   (assoc child :path (conj (vec (butlast (:path child))) idx))))

               (mapv (fn [child]
                       (assoc child :path (conj (:path x) (last (:path child)))))
                     (remove nil? (:childs x)))))))

(defn- lpath
  ([p] (interleave (repeat :childs) p))
  ([layout p]
   (concat [:layouts layout] (lpath p))))

(defn- insert-childs-at-path [this p childs]
  (update-in this
             (butlast p)
             (fn [x]
               (mapcat #(if (vector? %) % [%])
                       (assoc x (last p) childs)))))

(defn- extend-path [this idx]
  (let [parent-path (:path this)]
    (assoc this
      :path
      (vec (concat (take idx parent-path)
                   [0]
                   (drop idx parent-path)))
      :childs
      (mapv #(extend-path % idx) (:childs this)))))

(defn- update-flex-prop [this k f & args]
  (update-in this
             [:flex-props k]
             (fn [x]
               (apply f (or x (get default-flex-props k)) args))))

;; tools components ---------------------------------------------------

(defn props-panel [open? focus]
  (when @open?
    [:div.flexprops
     {:style {:display :flex
              :flex-flow "row nowrap"
              :justify-content "center"}}
     [:div.parent-props
      {:style {:padding "0 15px 15px 15px"}}
      [:h3 {:style {:padding :10px
                    :text-align :center}}
       "parent props"]
      (doall
        (for [k [:flex-direction
                 :flex-wrap
                 :justify-content
                 :align-items
                 :align-content]]
          ^{:key k}
          [select-coll focus k]))]
     [:div.child-props
      {:style {:padding "0 15px 15px 15px"}}
      [:h3 {:style {:padding :10px
                    :text-align :center}}
       "child props"]
      (doall
        (for [k [:order
                 :flex-grow
                 :flex-shrink]]
          ^{:key k}
          [num-input focus k]))
      [select-coll focus :align-self]
      [text-input focus :flex-basis]]]))

(defn actions [layout focus props-panel?]
  [:div.actions
   {:style {:display :flex
            :flex-flow "row nowrap"
            :justify-content :center}}
   [action
    "add child"
    (fn []
      (swap! focus
             (fn [{p :path :as focus}]
               (update focus
                       :childs
                       (fn [childs]
                         (let [cnt (count childs)]
                           (conj childs {:flex-props {}
                                         :childs []
                                         :path (conj p cnt)})))))))]
   [action
    "kill"
    (fn []
      (swap! layout
             #(re-idx (assoc-in % (lpath (:path @focus)) nil))))]
   [action
    "spread"
    (fn []
      (let [p (lpath (:path @focus))]
        (swap! layout
               #(-> %
                    (assoc-in p nil)
                    (insert-childs-at-path p (:childs @focus))
                    re-idx))))]
   [action
    "wrap"
    (fn []
      (swap! focus
             (fn [f]
               {:flex-props default-flex-props
                :path (:path f)
                :childs [(extend-path f (count (:path f)))]})))]
   [action
    "size +"
    (fn []
      (swap! focus
             update-flex-prop
             :flex-grow
             +
             step))]

   [action
    "size -"
    (fn []
      (swap! focus
             update-flex-prop
             :flex-grow
             -
             step))]
   [action
    "order +"
    (fn []
      (swap! focus
             update-flex-prop
             :order
             inc))]

   [action
    "order -"
    (fn []
      (swap! focus
             update-flex-prop
             :order
             dec))]
   [action
    "switch dir"
    (fn []
      (swap! focus
             update-flex-prop
             :flex-direction
             #(if (= % "row")
               "column"
               "row")))]

   [:i.fa.fa-cog
    {:style {:font-size :22px
             :color :lightcoral
             :align-self :center
             :padding-left :10px}
     :on-click #(swap! props-panel? not)}]])

;; layout component ------------------------------------------------------

(defn rect [{:keys [flex-props path childs] :as this} focus-path]
  (let [focus? (= path @focus-path)]
    [:div {:class (str "rect" path)
           :style (merge default-flex-props
                         flex-props
                         {:display :flex
                          :padding (str default-pad "px")
                          :background-color "rgba(0,0,0,.1)"
                          :border-width (str border-size "px")
                          :border-style :solid
                          :border-color (if focus?
                                          "lightcoral"
                                          "rgba(0,0,0,.2)")})
           :on-click (fn [e]
                       (if focus?
                         (reset! focus-path [])
                         (reset! focus-path path))
                       (.stopPropagation e))}
     (when-let [xs (seq childs)]
       (doall
         (for [[idx cr] (map vector (range) xs)]
           ^{:key (:path cr)}
           [rect (get-in this [:childs idx]) focus-path])))]))

;; arrow based navigation and key handler ------------------------

(defn- nav-childs [state step]
  (update state
          :focus-path
          #(conj (vec (butlast %))
                 (mod (+ step (last %))
                      (count (:childs (get-in state (lpath (:current-layout state) (butlast %)))))))))

(defn- any-focused-element? []
  (pos? (count ($ "*:focus"))))

(defn register-key-events [state focus]
  (aset js/document
        "onkeydown"
        (fn [e]
          (when (and (:props-panel? @state)
                     (= 27 (.-which e)))
            (swap! state update :props-panel? not))
          (when-not (any-focused-element?)
            ;; navigation arrow keys
            (condp = (.-which e)
              37 (swap! state nav-childs -1)
              39 (swap! state nav-childs 1)
              38 (swap! state update :focus-path (comp vec butlast))
              40 (when (seq (:childs focus)) (swap! state update :focus-path conj 0))
              nil)))))

;; main component -----------------------------------------------

(defn layout-composer [{:keys [default-layout layouts] :as props}]
  (let [state (atom {:layouts (merge {:default default-layout}
                                     layouts)
                     :focus-path []
                     :current-layout :default
                     :props-panel? false})
        focus-path (r/cursor state [:focus-path])
        layout (reaction (r/cursor state [:layouts (:current-layout @state)]))
        props-panel? (r/cursor state [:props-panel?])
        focus (reaction (r/cursor state (lpath (:current-layout @state) (:focus-path @state))))]
    (r/create-class
      {:reagent-render
       (fn []
         [:div
          (merge {:style {:display :flex
                          :flex-flow "column nowrap"
                          :height :100vh
                          :width :100vw}}
                 (:wrapper props))
          [actions @layout @focus props-panel?]
          [props-panel props-panel? @focus]
          [rect @@layout focus-path]])
       :component-did-mount
       #(register-key-events state @focus)})))

(r/render [layout-composer {:default-layout empty-layout}]
          ($1 "#app"))

