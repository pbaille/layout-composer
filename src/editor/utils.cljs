(ns editor.utils)

(defn $ [s]
  (array-seq (js/document.querySelectorAll s)))

(defn $1 [s]
  (first ($ s)))

(defn tval [e]
  (.. e -target -value))

(defn styles [el]
  (js/window.getComputedStyle (r/dom-node el)))

(defn hover? [e]
  (= (e.parentElement.querySelector ":hover") e))
