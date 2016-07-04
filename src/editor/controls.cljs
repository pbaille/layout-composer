(ns editor.controls
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as r]
            [editor.components :as cs]
            [editor.css-props :as cp]
            [editor.utils :as eu]
            [rlayout.functions :as rlf]
            [rlayout.utils :as rlu]))

(def config
  (atom {:step 0.1}))

;; constraints ---------------------------------------------------

(defn constraint-editor [state]
  [:div.media-query.ui.input.labeled
   [cs/labeled-dropdown {:cursor (r/cursor state [0])
                         :options nil #_(kws->options (keys @rlf/constraint-type->fn))}]
   [:input
    {:type :number
     :placeholder "value"
     :value (second @state)
     :on-change (fn [e] (swap! state assoc 1 (int (eu/tval e))))}]
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

(defn response-editor [{:keys [responses idx] :as props}]
  (let [response (r/cursor responses [idx])]
    [:div.response-editor
     [move-response-buttons props]
     [:div.input.ui.labeled
      [:div.ui.label "Name: "]
      [:input
       {:type :text
        :value (name (:id @response))
        :on-change #(swap! response assoc :id (eu/tval %))}]]
     [constraints-editor (r/cursor response [:constraints])]]))

(defn responses [{:keys [xs current]}]
  (let [edited (r/atom nil)]
    (fn [{:keys [xs current]}]
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
        focus-reaction (reaction (r/cursor state (into [:layout] (rlf/lpath (:focus-path @state)))))]
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
                       :current (:current @focus)}]])))))

;; actions ---------------------------------------------------------------

(defn action [n click-handler]
  [:div.ui.button.purple
   {:on-click click-handler}
   n])

(defn actions [state]
  (let [layout (r/cursor state [:layout])
        focus-path (reaction (:focus-path @state))
        props-panel? (r/cursor state [:props-panel?])
        focus-reaction (reaction (r/cursor state (into [:layout] (rlf/lpath @focus-path))))]
    (fn []
      (let [focus @focus-reaction]
        [:div.actions
         {:style {:display :flex
                  :flex-flow "row nowrap"
                  :justify-content :center}}
         [:div.ui.buttons
          {:style {:padding "5px"}}
          [action "add child" #(swap! layout rlf/insert-child {:path @focus-path})]
          [action "kill" #(swap! layout rlf/kill @focus-path)]
          [action "spread" #(swap! layout rlf/spread @focus-path)]
          [action "wrap" #(swap! layout rlf/wrap @focus-path)]]
         [:div.ui.buttons
          {:style {:padding "5px"}}
          [action "size +" #(swap! focus cp/update-flex-prop :flex-grow + (:step @config))]
          [action "size -" #(swap! focus cp/update-flex-prop :flex-grow - (:step @config))]
          [action "order +" #(swap! focus cp/update-flex-prop :order inc)]
          [action "order -" #(swap! focus cp/update-flex-prop :order dec)]
          [action "switch dir" (fn [] (swap! focus cp/update-flex-prop :flex-direction
                                             #(if (= % "row") "column" "row")))]]

         [:i.fa.fa-cog
          {:style {:font-size :22px
                   :color :#db2828
                   :align-self :center
                   :padding-left :10px}
           :on-click #(swap! props-panel? not)}]]))))