(ns rlayout.editor
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as r :refer [atom]]
            [rlayout.functions :as lp]
            [rlayout.css-props :refer [css-props]]
            [semantic.sidebar :refer [sidebar]]))

(enable-console-print!)

(defn $ [s] (array-seq (js/document.querySelectorAll s)))
(defn $1 [s] (first ($ s)))
(defn tval [e] (.. e -target -value))
(defn styles [el] (js/window.getComputedStyle (r/dom-node el)))
(defn hover? [e] (= (e.parentElement.querySelector ":hover") e))

(defn dimensions-map [el]
  (let [styles (styles el)
        parse-px-str #(int (re-find #"[\d]*" %))]
    {:w (parse-px-str (.-width styles))
     :h (parse-px-str (.-height styles))}))

(defn idx-shift [v idx dir]
  (let [cnt (count v)]
    (condp = dir
      :left (if (zero? idx) v (assoc v (dec idx) (get v idx) idx (get v (dec idx))))
      :right (if (= (dec cnt) idx) v (assoc v (inc idx) (get v idx) idx (get v (inc idx)))))))

(defn first-idx [pred coll]
  (ffirst (filter (comp pred second) (map vector (range) coll))))

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

(defn kws->options [kws]
  (map #(hash-map :name (name (or % "")) :value %) kws))

(defn labeled-dropdown [{:keys [cursor on-change options placeholder label]}]
  (let [uid (gensym)]
    (r/create-class
      {:reagent-render
       (fn [{:keys [props cursor options placeholder label]}]
         [:div.ui.action
          [:div.ui.left.attached.button.blue {:style {:width :130px}} label]
          [:div.ui.right.attached.dropdown.button
           (merge {:id uid} props)
           [:span.text (or (and @cursor (name @cursor)) placeholder "type")]
           [:div.menu
            (doall
              (for [{:keys [value name] :as o} options]
                [:div.item
                 {:key [value name]
                  :data-value value}
                 name]))]]])
       :component-did-mount
       (fn [this]
         (let [on-change (:on-change (r/props this) reset!)]
           (.dropdown (js/$ (str "#" uid))
                      (clj->js {:onChange #(on-change cursor %)}))))})))

(defn select-coll [state k]
  (let [c (r/cursor state [:flex-props k])]
    (fn []
      [labeled-dropdown
       {:label (name k)
        :cursor c
        :props {:style {:width :120px}}
        :options (kws->options (get flex-opts k))
        :placeholder (name (get default-flex-props k))}])))

(def input-button-styles
  {:width :120px
   :border :none
   :background :#e0e1e2
   :box-shadow "0 0 0 1px rgba(34,36,38,.15)"})

(defn num-input [state k]
  (let [c (r/cursor state [:flex-props k])]
    [:div
     [:div.ui.labeled.input
      [:div.ui.left.attached.button.blue {:style {:width :130px}}
       (name k)]
      [:input.ui {:style input-button-styles
                  :on-change (fn [e] (reset! c (tval e)))
                  :type "number"
                  :value (or @c (get default-flex-props k))}]]]))

(defn text-input [state k]
  (let [c (r/cursor state [:flex-props k])]
    [:div.ui.labeled.input
     [:div.ui.left.attached.button.blue {:style {:width :130px}}
      (name k)]
     [:input {:style input-button-styles
              :on-change (fn [e] (reset! c (tval e)))
              :type "text"
              :value (or @c (get default-flex-props k))}]]))

(defn- update-flex-prop [this k f & args]
  (update-in this
             [:flex-props k]
             (fn [x]
               (apply f (or x (get default-flex-props k)) args))))



;; constraints ---------------------------------------------------

(defn constraint-editor [state]
  [:div.media-query.ui.input.labeled
   [labeled-dropdown {:cursor (r/cursor state [0])
                      :options (kws->options (keys @lp/constraint-type->fn))}]
   [:input
    {:type :number
     :placeholder "value"
     :value (second @state)
     :on-change (fn [e] (swap! state assoc 1 (int (tval e))))}]
   [:div.ui.button {:on-click #(reset! state nil)} "-"]])

(defn constraints-editor [xs]
  [:div.constraints-editor
   (doall
     (for [[idx] (map vector (range) (remove nil? @xs))]
       ^{:key (str "constraint_" idx)}
       [constraint-editor (r/cursor xs [idx])]))
   [:div.ui.button {:on-click (fn [] (swap! xs conj [nil nil]))} "+"]])

;; responses -----------------------------------------------------

(defn response [& [spec]]
  (merge
    {:id (gensym "res")
     :constraints []
     :layout (lp/layout)}
    spec))

(defn response-span [{:keys [state edited current idx]}]
  (let [edited? (= @edited idx)]
    [:div.ui.button
     {:class
      (str
        (when (and (not edited?) (= (:id state) current)) "red")
        (when edited? "purple"))
      :key (gensym)
      :on-click (fn []
                  (swap! edited
                         (fn [eidx]
                           (when-not (= eidx idx) idx))))}
     (name (:id state))]))

(defn move-response-buttons [{:keys [responses idx edited]}]
  [:div.response-idx-editor.ui.buttons
   [:button.ui.button
    {:key :left
     :on-click
     (fn []
       (let [edited-id (:id (get @responses @edited))]
         (swap! responses idx-shift idx :left)
         (reset! edited
                 (first-idx #(= edited-id (:id %)) @responses))))}
    "<"]
   [:button.ui.button.icon [:i.icon.ban]]
   [:button.ui.button
    {:key :right
     :on-click
     (fn []
       (let [edited-id (:id (get @responses @edited))]
         (swap! responses idx-shift idx :right)
         (reset! edited
                 (first-idx #(= edited-id (:id %)) @responses))))}
    ">"]])

(defn response-editor [{:keys [responses idx] :as props}]
  (let [response (r/cursor responses [idx])]
    [:div.response-editor
     [move-response-buttons props]
     [:div.input.ui.labeled
      [:div.ui.label "Name: "]
      [:input
       {:type :text
        :value (name (:id @response))
        :on-change #(swap! response assoc :id (tval %))}]]
     [constraints-editor (r/cursor response [:constraints])]]))

(defn responses [{:keys [xs current]}]
  (let [edited (r/atom nil)]
    (fn [{:keys [xs current]}]
      [:div.responses
       {:style {:width :100%}}
       [:div.responses-index.ui.buttons.fluid
        {:style {:width :400px
                 :margin :auto}}
        [:div.ui.button
         {:class (when (or (not current) (= :default current)) "red")}
         "default"]
        (doall (for [[idx r] (map vector (range) @xs)]
                 [response-span {:state r :edited edited :current current :idx idx}]))
        [:div.ui.button {:on-click #(swap! xs conj (response))} "+"]]
       (when-let [idx @edited]
         [response-editor {:responses xs
                           :idx idx
                           :edited edited}])])))

;; css props ---------------------------------------------------------------

(defn css-prop [state]
  (let [uid (gensym)
        type (reaction (when-let [t (first @state)] (name t)))
        value (reaction (second @state))]
    (r/create-class
      {:reagent-render
       (fn [] [:div.css-prop.ui.input.labeled
               {:style {:width :100%}}
               [:div.css-prop-type.label.ui.dropdown.search.icon.button
                {:id uid}
                [:span.text (or @type "type")]
                [:div.menu
                 (doall
                   (for [prop css-props]
                     [:div.item
                      {:key prop
                       :data-value prop}
                      (name prop)]))]]
               [:input {:type "text" :placeholder "value" :value @value :on-change #(swap! state assoc 1 (tval %))}]])
       :component-did-mount
       (fn [] (.dropdown (js/$ (str "#" uid))
                         (clj->js {:onChange (fn [v] (println v) (swap! state assoc 0 (keyword v)))})))})))

;; props panel -------------------------------------------------------------

(defn props-panel [open? focus]
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
     [:div.css-props
      {:style {:padding "0 15px 15px 15px"}}
      [:h3 {:style {:padding :10px
                    :text-align :center}}
       "css props"]
      (doall
        (for [[idx k] (map vector (range) (:css-props @focus))]
          ^{:key k}
          [css-prop (r/cursor focus [:css-props idx])]))]
     [responses {:xs (r/cursor focus [:responses])
                 :current (:current @focus)}]]))

;; actions ---------------------------------------------------------------

(defn action [n click-handler]
  [:div.ui.button.purple
   {:on-click click-handler}
   n])

(defn actions [layout focus focus-path props-panel?]

  [:div.actions
   {:style {:display :flex
            :flex-flow "row nowrap"
            :justify-content :center}}
   [:div.ui.buttons
    {:style {:padding "5px"}}
    [action "add child" #(swap! layout lp/insert-child {:path focus-path})]
    [action "kill" #(swap! layout lp/kill focus-path)]
    [action "spread" #(swap! layout lp/spread focus-path)]
    [action "wrap" #(swap! layout lp/wrap focus-path)]]
   [:div.ui.buttons
    {:style {:padding "5px"}}
    [action "size +" #(swap! focus update-flex-prop :flex-grow + step)]
    [action "size -" #(swap! focus update-flex-prop :flex-grow - step)]
    [action "order +" #(swap! focus update-flex-prop :order inc)]
    [action "order -" #(swap! focus update-flex-prop :order dec)]
    [action "switch dir" (fn [] (swap! focus update-flex-prop :flex-direction
                                       #(if (= % "row") "column" "row")))]]

   [:i.fa.fa-cog
    {:style {:font-size :22px
             :color :lightcoral
             :align-self :center
             :padding-left :10px}
     :on-click #(swap! props-panel? not)}]])

;; rlayout component ------------------------------------------------------

(defn simple-layout []
  (letfn [(respond [this]
            (let [layout (:layout (r/props this))
                  response (lp/respond @layout (dimensions-map this))
                  fp (:focus-path (r/props this))
                  fp? (get-in @layout (lp/lpath @fp))]
              (when-not (= @layout response)
                (when-not fp? (reset! fp []))
                (reset! layout response))))
          (upd [this]
            (aset js/window
                  "onresize"
                  (fn [_] (respond this)))

            (respond this))]
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

(defn test-sidebar [state]
  (let [local-state (r/atom {:open-sections #{}})]
    (fn []
      [:div.sidepanel
       {:style {:position :fixed
                :min-height :100vh
                :top 0
                :right 0
                :width :400px
                :background :purple}}])))

(defn layout-composer [{:keys [layout] :as props}]
  (let [state (atom {:layout (or layout (lp/layout))
                     :focus-path []
                     :props-panel? true})
        focus-path (r/cursor state [:focus-path])
        layout (reaction (r/cursor state [:layout]))
        props-panel? (r/cursor state [:props-panel?])
        focus (reaction (r/cursor state (into [:layout] (lp/lpath (:focus-path @state)))))]
    (r/create-class
      {:reagent-render
       (fn []
         [:div
          (merge {:style {:display :flex
                          :flex-flow "column nowrap"
                          :height :100vh
                          :width :100vw}}
                 (:wrapper props))
          [sidebar {:content (list)}]
          [actions @layout @focus @focus-path props-panel?]
          [props-panel props-panel? @focus]
          [simple-layout {:layout @layout
                          :focus-path focus-path}]])
       :component-did-mount
       #(register-key-events state)})))

(r/render [layout-composer {:layout (lp/clayout {:responses [{:id :res2
                                                              :constraints [[:min-width 800]]
                                                              :layout (lp/layout)}
                                                             {:id :res1
                                                              :constraints [[:min-width 500]]
                                                              :layout (lp/layout)}
                                                             ]})}]
          ($1 "#app"))