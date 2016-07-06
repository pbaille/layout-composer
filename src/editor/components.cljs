(ns editor.components
  (:require [reagent.core :as r]
            [editor.css-props :as cp]
            [editor.utils :as eu]))

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

(defn dropdown [{:keys [cursor on-change options placeholder label]}]
  (let [uid (gensym)]
    (r/create-class
      {:reagent-render
       (fn [{:keys [class props cursor options placeholder label]}]
         [:div.ui.dropdown
          (merge {:id uid :class class} props)
          [:span.text (or (and @cursor (name @cursor)) placeholder "type")]
          [:div.menu
           (doall
             (for [{:keys [value name] :as o} options]
               [:div.item
                {:key [value name]
                 :data-value value}
                name]))]])
       :component-did-mount
       (fn [this]
         (let [on-change (:on-change (r/props this) reset!)]
           (.dropdown (js/$ (str "#" uid))
                      (clj->js {:onChange #(on-change cursor %)}))))})))

(defn select-coll [state k]
  (let [c (r/cursor state [:style k])]
    (fn []
      [labeled-dropdown
       {:label (name k)
        :cursor c
        :props {:style {:width :120px}}
        :options (kws->options (get cp/flex-opts k))
        :placeholder (name (get cp/default-flex-props k))}])))

(def input-button-styles
  {:width :120px
   :border :none
   :background :#e0e1e2
   :box-shadow "0 0 0 1px rgba(34,36,38,.15)"})

(defn num-input [state k]
  (let [c (r/cursor state [:style k])]
    [:div
     [:div.ui.labeled.input
      [:div.ui.left.attached.button.blue {:style {:width :130px}}
       (name k)]
      [:input.ui {:style input-button-styles
                  :on-change (fn [e] (reset! c (eu/tval e)))
                  :type "number"
                  :value (or @c (get cp/default-flex-props k))}]]]))

(defn text-input [state k]
  (let [c (r/cursor state [:style k])]
    [:div.ui.labeled.input
     [:div.ui.left.attached.button.blue {:style {:width :130px}}
      (name k)]
     [:input {:style input-button-styles
              :on-change (fn [e] (reset! c (eu/tval e)))
              :type "text"
              :value (or @c (get cp/default-flex-props k))}]]))