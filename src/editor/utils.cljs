(ns editor.utils
  (:require [reagent.core :as r]
            [cljs.tools.reader :refer [read-string]]
            [cljs.js :refer [empty-state eval js-eval]]))

(defn $ [s]
  (array-seq (js/document.querySelectorAll s)))

(defn $1 [s]
  (first ($ s)))

(defn tval [e]
  (.. e -target -value))

(defn download [filename s]
  (let [e (js/document.createElement "a")]
    (set! (.-href e)
          (str "data:text/plain;charset=utf-8," (js/encodeURIComponent s)))
    (set! (.-download e)
          filename)
    (set! (.-display (.-style e)) "none")
    (.appendChild js/document.body e)
    (.click e)
    (.removeChild js/document.body e)))

(defn eval-str [s]
  (eval (empty-state)
        (read-string s)
        {:eval       js-eval
         :source-map true
         :context    :expr}
        (fn [result] result)))

(comment ((:value (eval-str "(fn [a] a)")) 45))

(defn file-input [{:keys [on-change id hidden?]}]
  [:input
   {:id id
    :style     (when hidden? {:display :none})
    :type      :file
    :on-change (fn [e]
                 (let [r (js/FileReader.)]
                   (.readAsBinaryString r
                                        (-> e .-target .-files (aget 0)))
                   (set! (.-onloadend r)
                         #(on-change (.-result r)))))}])

(defn styles [el]
  (js/window.getComputedStyle (r/dom-node el)))

(defn hover? [e]
  (= (e.parentElement.querySelector ":hover") e))

(defn dissoc-flex-props [styles]
  (dissoc styles
          :flex-direction
          :flex-wrap
          :justify-content
          :align-items
          :align-content
          :order
          :flex-grow
          :flex-shrink
          :flex-basis
          :align-self))
