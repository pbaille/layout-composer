(ns lcomp.editor
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as r :refer [atom]]
            [lcomp.pure2 :as lp]))

(enable-console-print!)

(defn $ [s] (array-seq (js/document.querySelectorAll s)))
(defn $1 [s] (first ($ s)))
(defn tval [e] (.. e -target -value))
(defn styles [el] (js/window.getComputedStyle (r/dom-node el)))
(defn dimensions-map [el]
  (let [styles (styles el)
        parse-px-str #(int (re-find #"[\d]*" %))]
    {:w (parse-px-str (.-width styles))
     :h (parse-px-str (.-height styles))}))

;; defaults ----------------------------------------------------------

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

(def button-styles
  {:padding :5px
   :margin "10px 4px"
   :background :lightgrey
   :border-radius :4px})

(defn button [props txt]
  [:span (update props :style merge button-styles) txt])

(defn action [n click-handler]
  [:div {:style button-styles
         :on-click click-handler}
   n])

(defn- update-flex-prop [this k f & args]
  (update-in this
             [:flex-props k]
             (fn [x]
               (apply f (or x (get default-flex-props k)) args))))

;; tools components ---------------------------------------------------

(defn props-panel [open? focus]
  (println "rerender props panel")
  (when @open?
    [:div.flexprops
     {:style {:display :flex
              :flex-flow "row wrap"
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
      [text-input focus :flex-basis]]
     [:div.responses
      {:style {:flex-basis :100%
               :display :flex
               :flex-flow "row nowrap"
               :justify-content :center}}
      [:span {:style (merge button-styles
                            (if (or (not (:current @focus)) (= :default (:current @focus)))
                              {:background "lightcoral"
                               :color "white"}))}
       "default"]
      (let [rs (:responses @focus)]
        (doall (for [r rs]
                 [:span {:style (merge button-styles
                                       (if (= (:id r) (:current @focus))
                                         {:background "lightskyblue"
                                          :color "white"}))
                         :key (gensym)}
                  (name (:id r))])))
      [button {:on-click } "+"]]]))

(defn actions [layout focus focus-path props-panel?]

  [:div.actions
   {:style {:display :flex
            :flex-flow "row nowrap"
            :justify-content :center}}
   [action "add child" #(swap! layout lp/insert-child {:path focus-path})]
   [action "kill" #(swap! layout lp/kill {:path focus-path})]
   [action "spread" #(swap! layout lp/spread {:path focus-path})]
   [action "wrap" #(swap! layout lp/wrap {:path focus-path})]
   [action "size +" #(swap! focus update-flex-prop :flex-grow + step)]
   [action "size -" #(swap! focus update-flex-prop :flex-grow - step)]
   [action "order +" #(swap! focus update-flex-prop :order inc)]
   [action "order -" #(swap! focus update-flex-prop :order dec)]
   [action "switch dir" (fn [] (swap! focus update-flex-prop :flex-direction
                                      #(if (= % "row") "column" "row")))]

   [:i.fa.fa-cog
    {:style {:font-size :22px
             :color :lightcoral
             :align-self :center
             :padding-left :10px}
     :on-click #(swap! props-panel? not)}]])

;; layout component ------------------------------------------------------

(defn simple-layout []
  (letfn [(upd [this]
            (aset js/window
                  "onresize"
                  (fn [_] (respond this)))

            (let [layout (:layout (r/props this))
                  response (lp/respond @layout (dimensions-map this))
                  fp (:focus-path (r/props this))
                  fp? (get-in @layout (lp/lpath @fp))]
              (when-not (= @layout response)
                (when-not fp? (reset! fp []))
                (reset! layout response))))]
    (r/create-class
      {:reagent-render
       (fn [{:keys [layout focus-path]}]
         (let [{:keys [flex-props path childs] :as this} @layout
               focus? (= path @focus-path)]
           [:div {:class (apply str "rect" (interpose "." path))
                  :style (merge default-flex-props
                                flex-props
                                {:display :flex
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
                  ^{:key (gensym)}
                  [simple-layout {:layout (r/cursor layout [:childs idx])
                                  :focus-path focus-path}])))]))
       :component-did-update upd
       :component-did-mount upd})))

;; arrow based navigation and key handler ------------------------

(defn- nav-childs [state step]
  (update state
          :focus-path
          #(conj (lp/butlastv %)
                 (mod (+ step (last %))
                      (count (get-in (:layout state) (conj (lp/lpath (lp/butlastv %)) :childs)))))))

(defn- any-focused-element? []
  (pos? (count ($ "*:focus"))))

(defn register-key-events [state]
  (aset js/document
        "onkeydown"
        (fn [e]
          (let [{:keys [props-panel? layout focus-path]} @state]
            (when (and props-panel? (= 27 (.-which e)))
              (swap! state update :props-panel? not))
            (when-not (any-focused-element?)
              ;; navigation arrow keys
              (let [has-childs? (seq (get-in layout (conj (lp/lpath focus-path) :childs)))]
                (condp = (.-which e)
                  37 (swap! state nav-childs -1)
                  39 (swap! state nav-childs 1)
                  38 (swap! state update :focus-path lp/butlastv)
                  40 (when has-childs? (swap! state update :focus-path conj 0))
                  nil)))))))

;; main component -----------------------------------------------

(defn layout-composer [{:keys [layout] :as props}]
  (let [state (atom {:layout (or layout (lp/layout))
                     :focus-path []
                     :props-panel? true})
        focus-path (r/cursor state [:focus-path])
        layout (reaction (r/cursor state [:layout]))
        props-panel? (r/cursor state [:props-panel?])
        focus (reaction (r/cursor state (apply vector :layout (lp/lpath (:focus-path @state)))))]
    (r/create-class
      {:reagent-render
       (fn []
         [:div
          (merge {:style {:display :flex
                          :flex-flow "column nowrap"
                          :height :100vh
                          :width :100vw}}
                 (:wrapper props))
          [actions @layout @focus @focus-path props-panel?]
          [props-panel props-panel? @focus]
          [simple-layout {:layout @layout
                          :focus-path focus-path}]])
       :component-did-mount
       #(register-key-events state)})))

(r/render [layout-composer {:layout (lp/clayout {:responses [{:id :res2
                                                              :pred (fn [{:keys [w h]}] (> w 800))
                                                              :layout (lp/layout)}
                                                             {:id :res1
                                                              :pred (fn [{:keys [w h]}] (> w 500))
                                                              :layout (lp/layout)}
                                                             ]})}]
          ($1 "#app"))