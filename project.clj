(defproject lcomp "0.1.0-SNAPSHOT"
            :description "FIXME: write description"
            :url "http://example.com/FIXME"
            :license {:name "Eclipse Public License"
                      :url "http://www.eclipse.org/legal/epl-v10.html"}
            :dependencies [[org.clojure/clojure "1.8.0"]
                           [org.clojure/clojurescript "1.9.36"]
                           [org.clojure/core.async "0.1.346.0-17112a-alpha"]
                           [figwheel-sidecar "0.5.4-3"]
                           [reagent "0.5.1"]
                           [cljs-http "0.1.39"]
                           [garden "1.3.2"]
                           [prismatic/schema "1.1.2"]]
            :plugins [[lein-cljsbuild "1.1.0"]
                      [lein-garden "0.2.6"]]
            :source-paths ["src" "script"]
            :cljsbuild {:builds [{:id           "min"
                                  :source-paths ["src"]
                                  :compiler     {:main          'lcomp.core
                                                 :asset-path    "js/out"
                                                 :optimizations :advanced
                                                 :output-to     "resources/public/js/out/main.min.js"
                                                 :output-dir    "resources/public/js/out"}}]}
            :garden {:builds [{;; Optional name of the build:
                               :id "screen"
                               ;; Source paths where the stylesheet source code is
                               :source-paths ["src/styles"]
                               ;; The var containing your stylesheet:
                               :stylesheet lcomp.styles.core/screen
                               ;; Compiler flags passed to `garden.core/css`:
                               :compiler {;; Where to save the file:
                                          :output-to "resources/screen.css"
                                          ;; Compress the output?
                                          :pretty-print? false}}]})
