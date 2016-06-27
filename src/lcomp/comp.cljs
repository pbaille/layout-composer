(ns lcomp.comp
  (:require-macros [reagent.ratom :refer [reaction]])
  (:require [reagent.core :as r :refer [atom]]))

(defn $ [s] (array-seq (js/document.querySelectorAll s)))
(defn $1 [s] (first ($ s)))

(defn rclass [x]
  (r/create-class (if (fn? x)
                    {:reagent-render x}
                    x)))

(defn make-factory [state]
  (fn it [{:keys [make-props lifecycle render childs]}]
    (let [props (make-props state)
          comp (rclass (or lifecycle render))]
      (fn [_]
        [comp @props (doall (map #(it %) childs))]))))

(def state (r/atom {:text "yop" :foo "bar"}))

(defn div [& childs]
  {:make-props (fn [state] (reaction {:text (:text @state)}))
   :lifecycle {:reagent-render (fn [{:keys [text]} childs] (println "render div" childs) (into [:div text] childs))}
   :childs childs})

(def main (make-factory state))

(comment
  (swap! state assoc :text "baz")
  (swap! state assoc :foo "baz"))

(r/render [(main div) [[div] [div] [div]]]
          ($1 "#app"))