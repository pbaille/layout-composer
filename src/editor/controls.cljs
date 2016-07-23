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
  (map #(hash-map :name (name %) :value %) xs))

(defn constraint-editor [{state :constraint
                          cs    :constraints
                          idx   :idx
                          cm    :constraints-map}]
  [:div.media-query.ui.input.buttons.fluid.compact
   {:style {:margin-bottom :10px}}
   [cs/dropdown {:props   {:style {:width :44px :padding-left :10px}}
                 :class   "left attached button"
                 :cursor  (r/cursor state [0])
                 :options (kws->options (keys cm))}]
   [:input.ui.button.attached
    {:type        :number
     :placeholder "value"
     :value       (second @state)
     :on-change   (fn [e] (swap! state assoc 1 (int (eu/tval e))))}]
   [:div.ui.icon.button.negative.compact
    {:style    {:padding "10px 0"
                :width   0}
     :on-click #(swap! cs u/rem-idx idx)}
    [:i.trash.icon]]])

(defn constraints-editor [{xs :constraints cm :constraints-map}]
  [:div.constraints-editor
   (doall
     (for [[idx] (map vector (range) (remove nil? @xs))]
       ^{:key (str "constraint_" idx)}
       [constraint-editor {:constraint      (r/cursor xs [idx])
                           :constraints     xs
                           :idx             idx
                           :constraints-map cm}]))
   [:div
    {:style {:margin "10px 66px"}}
    [:button.ui.icon.button.circular {:on-click (fn [] (swap! xs conj [:min-width 0]))} [:i.icon.plus]]
    [:button.ui.icon.button.circular {:on-click (fn [] (swap! xs conj [:min-width 0]))} [:i.icon.checkmark]]
    [:button.ui.icon.button.circular {:on-click (fn [] (swap! xs conj [:min-width 0]))} [:i.icon.remove]]]])

;; responses -----------------------------------------------------



(defn move-response-buttons [{:keys [responses idx edited label]}]
  [:div.response-idx-editor.ui.icon.buttons.fluid
   (when label [:div.ui.button label])
   [:button.ui.button
    {:key :left
     :on-click
          (fn []
            (let [edited-id (:id (get @responses @edited))]
              (swap! responses rlu/idx-shift idx :left)
              (reset! edited
                      (rlu/first-idx #(= edited-id (:id %)) @responses))))}
    [:i.icon.chevron.down]]
   [:button.ui.button
    {:key :right
     :on-click
          (fn []
            (let [edited-id (:id (get @responses @edited))]
              (swap! responses rlu/idx-shift idx :right)
              (reset! edited
                      (rlu/first-idx #(= edited-id (:id %)) @responses))))}
    [:i.icon.chevron.up]]
   [:button.ui.button.icon [:i.icon.ban]]
   [:button.ui.button.icon [:i.icon.pencil]]])


(defn response-editor [{:keys [state responses idx constraints-map] :as props}]
  (let [response (r/cursor responses [idx])]
    [:div
     [:div.ui.header.small
      [:div.input.ui
       {:style {:width :100%}}
       [:input {:type      :text
                :value     (name (:id state))
                :on-change () #_(swap! response assoc :id (eu/tval %))}]]]
     [constraints-editor {:constraints     (r/cursor response [:constraints])
                          :constraints-map constraints-map}]]))

(defn response [{:keys [state edited current idx] :as props}]
  (let [edited? (= @edited idx)]
    [:div
     {:class    (when (= (:id state) current) "red")
      :key      (gensym)
      :on-click (fn []
                  (swap! edited
                         (fn [eidx]
                           (when-not (= eidx idx) idx))))}
     [:div.ui.container.fluid
      [move-response-buttons (assoc props :label (name (:id state)))]]]))

(defn responses-editor [_]
  (let [edited (r/atom nil)]
    (fn [{:keys [xs current constraints-map]}]
      [:div.responses.ui.segment
       {:style {:width :100%}}
       [:div.ui.header "Responses"
        [:div.ui.button.compact.right.floated
         {:style    {:width :40px}
          :on-click #(swap! xs conj (rlf/response))}
         [:i.icon.plus]]]
       [:div.ui.divider]
       (doall
         (interpose
           [:div.ui.divider]
           (for [[idx r] (map vector (range) @xs)]
             (if (= @edited idx)
               ^{:key [idx r]}
               [response-editor {:state           r
                                 :edited          edited
                                 :current         current
                                 :idx             idx
                                 :responses       xs
                                 :constraints-map constraints-map}]
               ^{:key [idx r]}
               [response {:state     r
                          :edited    edited
                          :current   current
                          :idx       idx
                          :responses xs}]))))])))

;; css props ---------------------------------------------------------------

(defn css-prop [{:keys [style-cursor]}]
  (let [uid (gensym)]
    (r/create-class
      {:reagent-render
       (fn [{:keys [prop value]}]
         [:div.css-prop.ui.input.labeled.fluid
          {:style {:margin-bottom :10px}}
          [:div.css-prop-type.label.ui.dropdown.search.icon.button
           {:id uid}
           [:span.text (name prop)]
           [:div.menu
            (doall
              (for [prop cp/css-props]
                [:div.item
                 {:key        prop
                  :data-value prop}
                 (name prop)]))]]
          [:input {:type        "text"
                   :placeholder "value"
                   :value       value
                   :on-change   #(swap! style-cursor assoc prop (eu/tval %))}]])
       :component-did-mount
       (fn [this]
         (.dropdown (js/$ (str "#" uid))
                    (clj->js {:onChange
                              (fn [v]
                                (swap! style-cursor
                                       #(-> %
                                            (dissoc (:prop (r/props this)))
                                            (assoc (keyword v) (:value (r/props this))))))})))})))

;; props panel -------------------------------------------------------------

(defn props-panel [state]
  (let [open? (reaction (:props-panel? @state))
        focus-reaction (reaction (r/cursor state (into [:layout] (rlf/lpath (:focus-path @state)))))
        constraints-map (reaction (get-in @state [:env :constraints-map]))]
    (fn []
      (let [focus @focus-reaction]
        (when @open?
          [:div.flexprops.ui.segment
           [:div.ui.header "CSS"]
           [:div.ui.divider]
           [:div.parent-props
            [:div.ui.medium.header "Flex parent"]
            (doall
              (for [k [:flex-direction
                       :flex-wrap
                       :justify-content
                       :align-items
                       :align-content]]
                ^{:key k}
                [cs/select-coll focus k]))]
           [:div.ui.divider]
           [:div.child-props
            [:div.ui.medium.header "Flex child"]
            (doall
              (for [k [:order
                       :flex-grow
                       :flex-shrink]]
                ^{:key k}
                [cs/num-input focus k]))
            [cs/select-coll focus :align-self]
            [cs/text-input focus :flex-basis]]
           [:div.ui.divider]
           [:div.child-props
            [:div.ui.medium.header "Props"
             [:div.ui.button.compact.right.floated
              {:style    {:width :40px}
               :on-click #(swap! focus update :style assoc :prop "value")}
              [:i.icon.plus]]]
            (doall
              (for [[k v] (eu/dissoc-flex-props (:style @focus))]
                ^{:key k}
                [css-prop {:style-cursor (r/cursor focus [:style])
                           :prop         k
                           :value        v}]))]])))))

(defn responses [state]
  (let [focus-reaction (reaction (r/cursor state (into [:layout] (rlf/lpath (:focus-path @state)))))
        constraints-map (reaction (get-in @state [:env :constraints-map]))]
    (fn []
      (let [focus @focus-reaction]
        [responses-editor
         {:xs              (r/cursor focus [:responses])
          :current         (:current @focus)
          :constraints-map @constraints-map}]))))

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
        [:div.ui.segment
         [:div.ui.header "Actions"]
         [:div.ui.divider]
         [:div
          [:div.ui.buttons.compact.purple.fluid
           {:style {:margin-bottom :10px}}
           [action "Add" #_[:i.icon.plus]
            #(swap! layout rlf/insert-child {:path @focus-path})]
           [action "Kill" #_[:i.icon.ban]
            #(swap! layout rlf/kill @focus-path)]
           [action "Spread" #_[:i.icon.sign.out]
            #(swap! layout rlf/spread @focus-path)]
           [action "Wrap" #_[:i.icon.sign.in]
            #(swap! layout rlf/wrap @focus-path)]]
          [:div.ui.buttons.compact.purple
           [action [:i.icon.chevron.left]
            #(do (swap! layout rlf/mv @focus-path :left)
                 (swap! focus-path focus-path-shift @layout :left))]
           [action [:i.icon.chevron.right]
            #(do (swap! layout rlf/mv @focus-path :right)
                 (swap! focus-path focus-path-shift @layout :right))]
           [action [:i.icon.expand]
            #(swap! focus cp/update-flex-prop :flex-grow + (:step @config))]
           [action [:i.icon.compress]
            #(swap! focus cp/update-flex-prop :flex-grow - (:step @config))]
           (let [c (if (= (get-in @focus [:style :flex-direction]) "column")
                     "resize horizontal"
                     "resize vertical")]
             [action [:i.icon {:class c}]
              (fn [] (swap! focus cp/update-flex-prop :flex-direction
                            #(if (= % "row") "column" "row")))])]]]))))

(defn io [layout-cursor]
  [:div
   [:input#upload-layout
    {:style     {:display :none}
     :type      :file
     :on-change (fn [e]
                  (let [r (js/FileReader.)]
                    (.readAsBinaryString
                      r
                      (-> e .-target .-files (aget 0)))
                    (set! (.-onloadend r)
                          (fn []
                            (println (type (.-result r)))
                            (println (.-result r))
                            (reset! layout-cursor
                                    (cljs.reader/read-string (.-result r)))))))}]
   [:div.ui.buttons.fluid
    [:div.ui.button
     {:on-click #(eu/download "layout.edn" @layout-cursor)}
     "Download"]
    [:div.ui.button
     {:on-click #(.click (eu/$1 "#upload-layout"))}
     "Upload"]]])

(defn sidepanel [state]
  (let [open? (r/cursor state [:props-panel?])
        layout-cursor (r/cursor state [:layout])]
    (fn []
      (if @open?
        [:div
         {:style {:overflow-y :scroll
                  :padding    :15px
                  :background :white
                  :position   :fixed
                  :top        0
                  :min-height :100vh
                  :left       (if true 0 500)
                  :width      :330px
                  :animation  "left linear .5s"}}
         [:i.remove.icon {:on-click #(reset! open? false)}]
         [actions state]
         [props-panel state]
         [responses state]
         [io layout-cursor]]
        [:div {:style {:position      :fixed
                       :top           :50%
                       :left          -25
                       :height        :50px
                       :width         :50px
                       :padding       "15px 5px 5px 27px"
                       :border-radius :25px
                       :background    :white}}
         [:i.unordered.list.icon
          {:on-click #(reset! open? true)}]]))))

