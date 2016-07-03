(ns semantic.sidebar
  (:require [reagent.core :as r]))

(defn sidebar [{:keys [content]}]
  (let []
    (r/create-class
      {:reagent-render (fn [{:keys [content]}]
                         [:div.ui.sidebar.vertical.menu
                          content])
       :component-did-mount (fn [this]
                              (println "did-mount")
                              (.addEventListener
                                     js/document
                                     "onkeydown"
                                     (fn [e]
                                       (println "yop")
                                       (println (.-which e))
                                       (case (.-which e)
                                         nil)

                                       #_(.sidebar (r/dom-node this)))))})))