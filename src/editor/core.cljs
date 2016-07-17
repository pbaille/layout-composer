(ns editor.core
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [rlayout.utils :as u]
            [rlayout.functions :as rlf]
            [rlayout.component :as rlc]
            [editor.controls :as ec]
            [editor.utils :as eu]
            [reagent.core :as r]))

(enable-console-print!)

;; key events ---------------------------------------------------------

(defn- nav-childs [state step]
  (update state
          :focus-path
          #(conj (u/butlastv %)
                 (mod (+ step (last %))
                      (count (get-in (:layout state) (rlf/brothers-path %)))))))

(defn- any-focused-element? []
  (pos? (count (eu/$ "*:focus"))))

(defn register-key-events [state]
  (aset js/document
        "onkeydown"
        (fn [e]
          (let [{:keys [props-panel? layout focus-path]} @state]
            (when (and props-panel? (= 27 (.-which e)))
              (swap! state update :props-panel? not))
            (when-not (any-focused-element?)
              ;; navigation arrow keys
              (let [has-childs? (seq (get-in layout (conj (rlf/lpath focus-path) :childs)))]
                (condp = (.-which e)
                  37 (swap! state nav-childs -1)
                  39 (swap! state nav-childs 1)
                  38 (swap! state update :focus-path u/butlastv)
                  40 (when has-childs? (swap! state update :focus-path conj 0))
                  nil)))))))

;; main ---------------------------------------------------------------

(defn render-focused [focus-path]
  (let [focus (eu/$1 (str "[data-path=\""
                          (apply str (interpose "," focus-path))
                          "\"]"))]

    (doseq [e (seq (eu/$ ".layout"))]
      (aset (.-style e)
            "borderColor"
            "rgba(0,0,0,.1)"))

    (aset (.-style focus)
          "borderColor"
          "#db2828")))

(def default-env
  {:components-map {}
   :constraints-map rlc/default-constraints-map
   :state (r/atom {})
   :placeholder rlc/default-placeholder-component})

(defn layout-editor [{:keys [layout env]}]
  (let [env (merge default-env env)
        local-state (r/atom {:layout (or layout (rlf/layout))
                             :focus-path []
                             :props-panel? true
                             :env env})
        layout (r/cursor local-state [:layout])
        focus-path (reaction (:focus-path @local-state))]
    (r/create-class
      {:reagent-render
       (fn []
         (:focus-path @local-state)
         [:div
          {:style {:display :flex
                   :justify-content :center
                   :align-content :center
                   :flex-flow "column nowrap"
                   :min-height :100vh
                   :min-width :100vw}}
          #_[ec/actions local-state]
          #_[ec/props-panel local-state]
          [ec/sidepanel local-state]
          [rlc/layout-comp {:layout layout :env env}]])
       :component-did-update
       #(render-focused @focus-path)
       :component-did-mount
       (fn [_]
         (render-focused @focus-path)
         (register-key-events local-state))})))

(r/render [layout-editor
           {:layout
            (rlf/layout
              {:responses [{:id :res2
                            :constraints [[:min-width 800]]
                            :layout (rlf/layout)}
                           {:id :res1
                            :constraints [[:min-width 500]]
                            :layout (rlf/layout)}]})}]
          (eu/$1 "#app"))