(ns editor.css-props
  (:require [rlayout.utils :as u]))

(def default-flex-props
  {:flex-direction "row"
   :flex-wrap "nowrap"
   :justify-content "flex-start"
   :align-items "stretch"
   :align-content "stretch"
   :order 1
   :flex-grow 1
   :flex-shrink 0
   :flex-basis "auto"
   :align-self "auto"})

(defn update-flex-prop [this k f & args]
  (u/update-in this
               [:style k]
               (fn [x]
               (apply f (or x (get default-flex-props k)) args))))

(def flex-parent-opts
  {:flex-direction #{:row :column}
   :flex-wrap #{:wrap :nowrap :wrap-reverse}
   :justify-content #{:flex-start
                      :flex-end
                      :center
                      :space-between
                      :space-around}
   :align-items #{:flex-start
                  :flex-end
                  :center
                  :baseline
                  :stretch}
   :align-content #{:flex-start
                    :flex-end
                    :center
                    :stretch
                    :space-between
                    :space-around}})

(def flex-child-opts
  {:order :int
   :flex-grow :float
   :flex-shrink :float
   :flex-basis #{:auto
                 {:val :float
                  :unit [:% :px :em :rem :vh :vw :vmin :vmax :ex :cm :mm :in :pt :pc]}}
   :align-self #{:auto
                 :flex-start
                 :flex-end
                 :center
                 :baseline
                 :stretch}})

(def flex-opts
  (merge flex-child-opts
         flex-parent-opts))

(def css-props
  [:all
   :animation
   :animation-delay
   :animation-direction
   :animation-duration
   :animation-fill-mode
   :animation-iteration-count
   :animation-name
   :animation-play-state
   :animation-timing-function
   :backface-visibility
   :background
   :background-attachment
   :background-blend-mode
   :background-clip
   :background-color
   :background-image
   :background-origin
   :background-position
   :background-repeat
   :background-size
   :border
   :border-bottom
   :border-bottom-color
   :border-bottom-left-radius
   :border-bottom-right-radius
   :border-bottom-style
   :border-bottom-width
   :border-collapse
   :border-color
   :border-image
   :border-image-outset
   :border-image-repeat
   :border-image-slice
   :border-image-source
   :border-image-width
   :border-left
   :border-left-color
   :border-left-style
   :border-left-width
   :border-radius
   :border-right
   :border-right-color
   :border-right-style
   :border-right-width
   :border-spacing
   :border-style
   :border-top
   :border-top-color
   :border-top-left-radius
   :border-top-right-radius
   :border-top-style
   :border-top-width
   :border-width
   :bottom
   :box-shadow
   :box-sizing
   :caption-side
   :clear
   :clip
   :color
   :column-count
   :column-fill
   :column-gap
   :column-rule
   :column-rule-color
   :column-rule-style
   :column-rule-width
   :column-span
   :column-width
   :columns
   :content
   :counter-increment
   :counter-reset
   :cursor
   :direction
   :display
   :empty-cells
   :filter
   :float
   :font
   :font-family
   :font-size
   :font-size-adjust
   :font-stretch
   :font-style
   :font-variant
   :font-weight
   :hanging-punctuation
   :height
   :left
   :letter-spacing
   :line-height
   :list-style
   :list-style-image
   :list-style-position
   :list-style-type
   :margin
   :margin-bottom
   :margin-left
   :margin-right
   :margin-top
   :max-height
   :max-width
   :min-height
   :min-width
   :nav-down
   :nav-index
   :nav-left
   :nav-right
   :nav-up
   :opacity
   :outline
   :outline-color
   :outline-offset
   :outline-style
   :outline-width
   :overflow
   :overflow-x
   :overflow-y
   :padding
   :padding-bottom
   :padding-left
   :padding-right
   :padding-top
   :page-break-after
   :page-break-before
   :page-break-inside
   :perspective
   :perspective-origin
   :position
   :quotes
   :resize
   :right
   :tab-size
   :table-layout
   :text-align
   :text-align-last
   :text-decoration
   :text-decoration-color
   :text-decoration-line
   :text-decoration-style
   :text-indent
   :text-justify
   :text-overflow
   :text-shadow
   :text-transform
   :top
   :transform
   :transform-origin
   :transform-style
   :transition
   :transition-delay
   :transition-duration
   :transition-property
   :transition-timing-function
   :unicode-bidi
   :vertical-align
   :visibility
   :white-space
   :width
   :word-break
   :word-spacing
   :word-wrap
   :z-index])

