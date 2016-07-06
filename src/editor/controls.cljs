(ns editor.controls
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as r]
            [editor.components :as cs]
            [editor.css-props :as cp]
            [editor.utils :as eu]
            [rlayout.functions :as rlf]
            [rlayout.utils :as rlu]
            [rlayout.utils :as u]))

(def config
  (atom {:step 0.1}))

;; constraints ---------------------------------------------------

(defn kws->options [xs]
  (println "kws->opts" xs)
  (map #(hash-map :name (name %) :value %) xs))

(defn constraint-editor [{state :constraint
                          cs :constraints
                          idx :idx
                          cm :constraints-map}]
  (println cm)
  [:div.media-query.ui.input.buttons
   [cs/dropdown {:class "left attached button"
                 :cursor (r/cursor state [0])
                 :options (kws->options (keys cm))}]
   [:input.ui.button.attached
    {:type :number
     :placeholder "value"
     :value (second @state)
     :on-change (fn [e] (swap! state assoc 1 (int (eu/tval e))))}]
   [:div.ui.button.right.attached.negative
    {:on-click #(swap! cs u/rem-idx idx)}
    [:i.trash.icon]]])

(defn constraints-editor [{xs :constraints cm :constraints-map}]
  [:div.constraints-editor
   (doall
     (for [[idx] (map vector (range) (remove nil? @xs))]
       ^{:key (str "constraint_" idx)}
       [constraint-editor {:constraint (r/cursor xs [idx])
                           :constraints xs
                           :idx idx
                           :constraints-map cm}]))
   [:div.ui.button {:on-click (fn [] (swap! xs conj [:min-width 0]))} "+"]])

;; responses -----------------------------------------------------

(defn response [& [spec]]
  (merge
    {:id (gensym "res")
     :constraints []
     :layout (rlf/layout)}
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
         (swap! responses rlu/idx-shift idx :left)
         (reset! edited
                 (rlu/first-idx #(= edited-id (:id %)) @responses))))}
    "<"]
   [:button.ui.button.icon [:i.icon.ban]]
   [:button.ui.button
    {:key :right
     :on-click
     (fn []
       (let [edited-id (:id (get @responses @edited))]
         (swap! responses rlu/idx-shift idx :right)
         (reset! edited
                 (rlu/first-idx #(= edited-id (:id %)) @responses))))}
    ">"]])

(defn response-editor [{:keys [responses idx constraints-map] :as props}]
  (let [response (r/cursor responses [idx])]
    [:div.response-editor
     {:style
      {:display :flex
       :flex-flow "column nowrap"
       :justify-content :center
       :align-content :center
       :border-top "5px solid #a333c8"}}
     [move-response-buttons props]
     [:div.input.ui.labeled
      [:div.ui.label "Name: "]
      [:input
       {:type :text
        :value (name (:id @response))
        :on-change #(swap! response assoc :id (eu/tval %))}]]
     [constraints-editor {:constraints (r/cursor response [:constraints])
                          :constraints-map constraints-map}]]))

(defn responses [_]
  (let [edited (r/atom nil)]
    (fn [{:keys [xs current constraints-map]}]
      [:div.responses
       {:style {:width :100%}}
       [:div.responses-index.ui.buttons.fluid.top.attached
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
                           :edited edited
                           :constraints-map constraints-map}])])))

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
                   (for [prop cp/css-props]
                     [:div.item
                      {:key prop
                       :data-value prop}
                      (name prop)]))]]
               [:input {:type "text" :placeholder "value" :value @value :on-change #(swap! state assoc 1 (eu/tval %))}]])
       :component-did-mount
       (fn [] (.dropdown (js/$ (str "#" uid))
                         (clj->js {:onChange (fn [v] (println v) (swap! state assoc 0 (keyword v)))})))})))

;; props panel -------------------------------------------------------------

(defn props-panel [state]
  (let [open? (reaction (:props-panel? @state))
        focus-reaction (reaction (r/cursor state (into [:layout] (rlf/lpath (:focus-path @state)))))
        constraints-map (reaction (get-in @state [:env :constraints-map]))]
    (fn []
      (let [focus @focus-reaction]
        (when @open?
          [:div.flexprops
           {:style {:display :flex
                    :flex-flow "row wrap"
                    :justify-content "center"}}
           [:div.parent-props
            {:style {:padding "0 15px 15px 15px"}}
            [:h3 {:style {:padding :10px
                          :margin 0
                          :text-align :center}}
             "parent props"]
            (doall
              (for [k [:flex-direction
                       :flex-wrap
                       :justify-content
                       :align-items
                       :align-content]]
                ^{:key k}
                [cs/select-coll focus k]))]
           [:div.child-props
            {:style {:padding "0 15px 15px 15px"}}
            [:h3 {:style {:padding :10px
                          :margin 0
                          :text-align :center}}
             "child props"]
            (doall
              (for [k [:order
                       :flex-grow
                       :flex-shrink]]
                ^{:key k}
                [cs/num-input focus k]))
            [cs/select-coll focus :align-self]
            [cs/text-input focus :flex-basis]]
           #_[:div.css-props
              {:style {:padding "0 15px 15px 15px"}}
              [:h3 {:style {:padding :10px
                            :text-align :center}}
               "css props"]
              (doall
                (for [[idx k] (map vector (range) (:css-props @focus))]
                  ^{:key k}
                  [css-prop (r/cursor focus [:css-props idx])]))]
           [responses {:xs (r/cursor focus [:responses])
                       :current (:current @focus)
                       :constraints-map @constraints-map}]])))))

;; actions ---------------------------------------------------------------

(defn action [n click-handler]
  [:div.ui.button
   {:on-click click-handler}
   n])

(defn focus-path-shift [p l dir]
  (let [bp (rlf/brothers-path p)]
    (conj (u/butlastv p)
          (u/mod-shift (count (get-in l bp))
                       (last p)
                       dir))))

(defn actions [state]
  (let [layout (r/cursor state [:layout])
        focus-path (r/cursor state [:focus-path])
        props-panel? (r/cursor state [:props-panel?])
        focus-reaction (reaction (r/cursor state (into [:layout] (rlf/lpath @focus-path))))]
    (fn []
      (let [focus @focus-reaction]
        [:div.actions
         {:style {:display :flex
                  :flex-flow "row nowrap"
                  :justify-content :center}}
         [:div.ui.buttons.compact.purple
          {:style {:padding "5px"}}
          [action [:i.icon.plus]
           #(swap! layout rlf/insert-child {:path @focus-path})]
          [action [:i.icon.chevron.left]
           #(do (swap! layout rlf/mv @focus-path :left)
                (swap! focus-path focus-path-shift @layout :left))]
          [action [:i.icon.chevron.right]
           #(do (swap! layout rlf/mv @focus-path :right)
                (swap! focus-path focus-path-shift @layout :right))]
          [action [:i.icon.ban]
           #(swap! layout rlf/kill @focus-path)]
          [action [:i.icon.sign.out]
           #(swap! layout rlf/spread @focus-path)]
          [action [:i.icon.sign.in]
           #(swap! layout rlf/wrap @focus-path)]]
         [:div.ui.buttons.compact.purple
          {:style {:padding "5px"}}
          [action [:i.icon.expand]
           #(swap! focus cp/update-flex-prop :flex-grow + (:step @config))]
          [action [:i.icon.compress]
           #(swap! focus cp/update-flex-prop :flex-grow - (:step @config))]
          (let [c (if (= (get-in @focus [:style :flex-direction]) "column")
                    "resize horizontal"
                    "resize vertical")]
            [action [:i.icon {:class c}]
             (fn [] (swap! focus cp/update-flex-prop :flex-direction
                           #(if (= % "row") "column" "row")))])]

         [:i.fa.fa-cog
          {:style {:font-size :22px
                   :color :#db2828
                   :align-self :center
                   :padding-left :10px}
           :on-click #(swap! props-panel? not)}]]))))

(defn sidepanel [state]
  (let [open? (reaction (:props-panel? @state))]
    (fn []
      [:div
       {:style {:position :fixed
                :top 0
                :min-height :100vh
                :left (if true 0 500)
                :width :400px
                :animation "left linear .5s"}}
       [actions state]
       [props-panel state]])))