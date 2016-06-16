(ns lcomp.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as r :refer [atom]]
            [clojure.walk :as w :refer [postwalk]]
            [schema.core :as s]))

(enable-console-print!)

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

(def state (atom {:flex-props default-flex-props :path [] :childs []}))
(def k (cljs.core/atom 0))
(def focus (atom state))
(def show-tools (atom false))
(def default-pad "10px")
(def step 0.1)
(def border-size 6)

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

(defn select-coll [state k]
  (let [c (r/cursor state [:flex-props k])]
    [:div
     {:style {:display :flex
              :flex-flow "row nowrap"
              :justify-content "space-between"
              :padding :3px}}
     [:span {:style {:padding "0 10px"}}
      (name k)]
     (into [:select {:style {:width :100px}
                     :value @c
                     :on-change (fn [e] (reset! c (.. e -target -value)))}]
           (doall
             (for [o (get flex-opts k)]
               [:option {:value (name o)
                         :key o}
                (name o)])))]))

(defn num-input [state k]
  (let [c (r/cursor state [:flex-props k])]
    [:div {:style {:display :flex
                   :flex-flow "row nowrap"
                   :justify-content "space-between"
                   :padding :3px}}
     [:span {:style {:padding "0 10px"}}
      (name k)]
     [:input {:style {:width :100px}
              :on-change (fn [e] (reset! c (.. e -target -value)))
              :type "number"
              :value @c}]]))

(defn text-input [state k]
  (let [c (r/cursor state [:flex-props k])]
    [:div {:style {:display :flex
                   :flex-flow "row nowrap"
                   :justify-content "space-between"
                   :padding :3px}}
     [:span {:style {:padding "0 10px"}}
      (name k)]
     [:input {:style {:width :100px}
              :on-change (fn [e] (reset! c (.. e -target -value)))
              :type "text"
              :value @c}]]))

(defn flex-props []
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
        [select-coll @focus k]))]
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
        [num-input @focus k]))
    [select-coll @focus :align-self]
    [text-input @focus :flex-basis]]])

(defn action [n click-handler]
  [:div {:style {:padding :5px
                 :margin "10px 4px"
                 :background :lightgrey
                 :border-radius :4px}
         :on-click click-handler} n])

(defn re-idx [x]
  (let [path (:path x)]
    (assoc x :childs
             (mapv
               (fn [[idx child]]
                 (let [new-path (conj
                                  (vec (butlast (:path child)))
                                  idx)]
                   (re-idx (assoc
                             child
                             :path
                             new-path))))
               (partition 2
                          (interleave (range)
                                      (mapv (fn [child]
                                              (assoc child :path (conj (:path x) (last (:path child)))))
                                            (remove nil? (:childs x)))))))))

(defn lpath [p]
  (interleave (repeat :childs) p))

(defn insert-childs-at-path [this p childs]
  (update-in this (butlast p) (fn [x] (mapcat #(if (vector? %) % [%]) (assoc x (last p) childs)))))

(defn extend-path [this idx]
  (let [parent-path (:path this)]
    (assoc this
      :path
      (vec (concat (take idx parent-path)
                   [0]
                   (drop idx parent-path)))
      :childs
      (mapv #(extend-path % idx) (:childs this)))))

(defn actions []
  [:div.actions
   {:style {:display :flex
            :flex-flow "row nowrap"
            :justify-content :center}}
   [action
    "add child"
    (fn []
      (swap! @focus
             #(update %
                      :childs
                      (fn [childs]
                        (let [cnt (count childs)]
                          (conj childs {:flex-props default-flex-props
                                        :childs []
                                        :path (conj (:path %) cnt)}))))))]
   [action
    "kill"
    (fn []
      (reset! (r/cursor state (lpath (:path @@focus))) nil)
      (swap! state re-idx))]
   [action
    "spread"
    (fn []
      (let [childs (:childs @@focus)
            p (lpath (:path @@focus))]
        (reset! (r/cursor state p) nil)
        (swap! state insert-childs-at-path p childs)
        (swap! state re-idx)))]
   [action
    "wrap"
    (fn []
      (swap! @focus
             (fn [f]
               {:flex-props default-flex-props
                :path (:path f)
                :childs [(extend-path f (count (:path f)))]})))]
   [action
    "size +"
    (fn []
      (swap! @focus
             update-in
             [:flex-props :flex-grow]
             +
             step))]

   [action
    "size -"
    (fn []
      (swap! @focus
             update-in
             [:flex-props :flex-grow]
             -
             step))]
   [action
    "order +"
    (fn []
      (swap! @focus
             update-in
             [:flex-props :order]
             +
             step))]

   [action
    "order -"
    (fn []
      (swap! @focus
             update-in
             [:flex-props :order]
             -
             step))]
   [action
    "switch dir"
    (fn []
      (swap! @focus
             update-in
             [:flex-props :flex-direction]
             #(if (= % "row")
               "column"
               "row")))]

   [:i.fa.fa-cog
    {:style {:font-size :22px
             :color :lightcoral
             :align-self :center
             :padding-left :10px}
     :on-click #(swap! show-tools not)}]])

(defn rect [ref]
  (let [{:keys [flex-props childs]} @ref]
    [:div.rect {:style (merge flex-props
                              {:display :flex
                               :padding (str default-pad "px")
                               :background-color "rgba(0,0,0,.1)"
                               :border-width (str border-size "px")
                               :border-style :solid
                               :border-color (if (= (:path @ref) (:path @@focus))
                                         "lightcoral"
                                         "rgba(0,0,0,.2)")})
                :on-click (fn [e]
                            (if (= @focus ref)
                              (reset! focus state)
                              (reset! focus ref))
                            (.stopPropagation e))}
     (when-let [xs (seq childs)]
       (doall
         (for [[idx cr] (map vector (range) xs)]
           ^{:key (swap! k inc)}
           [rect (r/cursor ref [:childs idx])])))]))

(defn main []
  [:div {:style {:display :flex
                 :flex-flow "column nowrap"
                 :height :100vh
                 :width :100vw}}
   [actions]
   (when @show-tools
     [flex-props])
   [rect state 30]])

(aset js/document
      "onkeydown"
      (fn [e]
        (when (and @show-tools (= 27 (.-which e)))
          (swap! show-tools not))
        (when-not @show-tools
          ;; navigation arrow keys
          (condp = (.-which e)
            37 (reset! focus
                       (r/cursor state
                                 (lpath
                                   (conj (vec (butlast (:path @@focus)))
                                         (mod (dec (last (:path @@focus)))
                                              (count (:childs (get-in @state (lpath (butlast (:path @@focus)))))))))))
            39 (reset! focus
                       (r/cursor state
                                 (lpath
                                   (conj (vec (butlast (:path @@focus)))
                                         (mod (inc (last (:path @@focus)))
                                              (count (:childs (get-in @state (lpath (butlast (:path @@focus)))))))))))
            38 (reset! focus
                       (r/cursor state (lpath (vec (butlast (:path @@focus))))))

            40 (when (seq (:childs @@focus))
                 (reset! focus
                         (r/cursor state (lpath (conj (:path @@focus) 0)))))
            nil))))

(r/render-component [main]
                    (.getElementById js/document "app"))

