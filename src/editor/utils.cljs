(ns editor.utils
  (:require [reagent.core :as r]))

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
