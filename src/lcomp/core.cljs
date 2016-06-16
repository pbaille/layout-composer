(ns lcomp.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as r :refer [atom]]
            [clojure.walk :as w :refer [postwalk]]
            [schema.core :as s]))

(enable-console-print!)

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

(def empty-layout {:flex-props default-flex-props :path [] :childs []})

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
    (fn []
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

(defn props-panel [focus]
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
    [text-input focus :flex-basis]]])

(defn action [n click-handler]
  [:div {:style {:padding :5px
                 :margin "10px 4px"
                 :background :lightgrey
                 :border-radius :4px}
         :on-click click-handler}
   n])

(defn re-idx [x]
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
                                          (remove nil? (:childs x))))))))

(defn lpath [p]
  (interleave (repeat :childs) p))

(defn insert-childs-at-path [this p childs]
  (update-in this
             (butlast p)
             (fn [x]
               (mapcat #(if (vector? %) % [%])
                       (assoc x (last p) childs)))))

(defn extend-path [this idx]
  (let [parent-path (:path this)]
    (assoc this
      :path
      (vec (concat (take idx parent-path)
                   [0]
                   (drop idx parent-path)))
      :childs
      (mapv #(extend-path % idx) (:childs this)))))

(defn actions [state focus]
  (println @state @focus)
  [:div.actions
   {:style {:display :flex
            :flex-flow "row nowrap"
            :justify-content :center}}
   [action
    "add child"
    (fn []
      (println @focus)
      (swap! focus
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
      (swap! state
             update
             :layout
             #(re-idx (assoc-in % (lpath (:path @focus)) nil))))]
   [action
    "spread"
    (fn []
      (let [p (lpath (:path @focus))]
        (swap! state
               update
               :layout
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
             update-in
             [:flex-props :flex-grow]
             +
             step))]

   [action
    "size -"
    (fn []
      (swap! focus
             update-in
             [:flex-props :flex-grow]
             -
             step))]
   [action
    "order +"
    (fn []
      (swap! focus
             update-in
             [:flex-props :order]
             inc))]

   [action
    "order -"
    (fn []
      (swap! focus
             update-in
             [:flex-props :order]
             dec))]
   [action
    "switch dir"
    (fn []
      (swap! focus
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
     :on-click #(swap! state update :props-panel? not)}]])

(defn rect [{:keys [flex-props path childs] :as this} focus-path]
  (let [focus? (= path @focus-path)]
    [:div.rect {:style (merge flex-props
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

(defn focus [state]
  (r/cursor state (cons :layout (lpath (:focus-path @state)))))

(defn register-key-events [state]
  (aset js/document
        "onkeydown"
        (fn [e]
          (when (and (:props-panel? @state)
                     (= 27 (.-which e)))
            (swap! state update :props-panel? not))
          (let [focus @(focus state)]
            (when-not (:props-panel? @state)
              ;; navigation arrow keys
              (condp = (.-which e)
                37 (swap! state
                          update
                          :focus-path
                          #(conj (vec (butlast %))
                                 (mod (dec (last %))
                                      (count (:childs (get-in @state (cons :layout (lpath (butlast %)))))))))
                39 (swap! state
                          update
                          :focus-path
                          #(conj (vec (butlast %))
                                 (mod (inc (last %))
                                      (count (:childs (get-in @state (cons :layout (lpath (butlast %)))))))))
                38 (swap! state
                          update
                          :focus-path
                          (comp vec butlast))

                40 (when (seq (:childs focus))
                     (swap! state
                            update
                            :focus-path
                            conj
                            0))
                nil))))))

(defn layout-composer [{:keys [layout] :as props}]
  (let [state (atom {:layout layout
                     :focus-path []
                     :props-panel? false})
        focus-path (reaction (r/cursor state [:focus-path]))
        focus (reaction (r/cursor state (cons :layout (lpath (:focus-path @state)))))]
    (r/create-class
      {:reagent-render
       (fn [] [:div
               (merge {:style {:display :flex
                               :flex-flow "column nowrap"
                               :height :100vh
                               :width :100vw}}
                      (:wrapper props))
               [actions state @focus]
               (when (:props-panel? @state) [props-panel @focus])
               [rect (:layout @state) @focus-path]])
       :component-did-mount
       #(register-key-events state)})))

(r/render-component [layout-composer {:layout empty-layout}]
                    (.getElementById js/document "app"))

