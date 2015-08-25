(ns uxbox.shapes.rectangle
  (:require [uxbox.shapes.core :refer [Shape generate-transformation fill-menu actions-menu stroke-menu]]
            [uxbox.pubsub :as pubsub]
            [uxbox.icons :as icons]
            [uxbox.geometry :as geo]
            [uxbox.icons :as icons]
            [cljs.reader :as reader]
            [reagent.core :refer [atom]]))

(def rectangle-menu {:name "Size and position"
                     :icon icons/infocard
                     :key :options
                     :options [{:name "Position" :inputs [{:name "X" :type :number :shape-key :x :value-filter int}
                                                          {:name "Y" :type :number :shape-key :y :value-filter int}]}
                               {:name "Size" :inputs [{:name "Width" :type :number :shape-key :width :value-filter int}
                                                      {:name "lock" :type :lock}
                                                      {:name "Height" :type :number :shape-key :height :value-filter int}]}]})

(defrecord Rectangle [x y width height rx ry fill fill-opacity stroke stroke-width stroke-opacity rotate]
  Shape
  (intersect [{:keys [x y width height]} px py]
    (and (>= px x)
         (<= px (+ x width))
         (>= py y)
         (<= py (+ y height))))

  (toolbar-coords [{:keys [x y width height]}]
    (let [vx (+ x width 50)
          vy (- y 50)]
      (geo/viewportcoord->clientcoord vx vy)))

  (shape->svg [{:keys [x y width height rx ry fill fill-opacity stroke stroke-width stroke-opacity rotate]}]
    [:rect
     {:x x
      :y y
      :width width
      :height height
      :fill fill
      :fillOpacity fill-opacity
      :rx rx
      :ry ry
      :stroke stroke
      :strokeWidth stroke-width
      :stroke-opacity stroke-opacity
      :rotate rotate
      :transform (generate-transformation {:rotate rotate :center {:x (+ x (/ width 2)) :y (+ y (/ height 2))}})}])

  (shape->selected-svg [{:keys [x y width height rx ry fill fill-opacity stroke stroke-width stroke-opacity rotate]}]
    [:g
     [:rect {:x (- x 4)
             :y (- y 4)
             :width (+ width 8)
             :height (+ height 8)
             :fill "transparent"
             :stroke "#4af7c3"
             :strokeWidth 2
             :strokeDasharray "5,5"
             :fill-opacity "0.5"
             :rotate rotate
             :transform (generate-transformation {:rotate rotate :center {:x (+ x (/ width 2)) :y (+ y (/ height 2))}})}]

      [:rect {:x (- x 8) :y (- y 8) :width 8 :height 8 :fill "#4af7c3" :fill-opacity "0.75"}]
      [:rect {:x (+ x width) :y (+ y height) :width 8 :height 8 :fill "#4af7c3" :fill-opacity "0.75"}]
      [:rect {:x (+ x width) :y (- y 8) :width 8 :height 8 :fill "#4af7c3" :fill-opacity "0.75"}]
      [:rect {:x (- x 8) :y (+ y height) :width 8 :height 8 :fill "#4af7c3" :fill-opacity "0.75"}]])

  (shape->drawing-svg [{:keys [x y]}]
    (let [coordinates (atom [[x y]])
          viewport-move (fn [state coord]
                          (reset! coordinates coord))]
      (pubsub/register-event :viewport-mouse-move viewport-move)
      (fn []
        (let [[mouseX mouseY] @coordinates
              [rect-x rect-y rect-width rect-height] (geo/coords->rect x y mouseX mouseY)]
          (if (and (> rect-width 0) (> rect-height 0))
            [:rect {:x rect-x :y rect-y :width rect-width :height rect-height
                    :style #js {:fill "transparent" :stroke "gray" :strokeDasharray "5,5"}}])))))

  (move-delta
    [{:keys [x y] :as shape} delta-x delta-y]
    (-> shape
        (assoc :x (+ x delta-x))
        (assoc :y (+ y delta-y))))

  (menu-info
    [shape]
    [rectangle-menu stroke-menu fill-menu actions-menu]))

(defn new-rectangle
  "Retrieves a line with the default parameters"
  [x y width height]
  (Rectangle. x y width height 0 0 "#cacaca" 1 "gray" 5 1 0))

(defn drawing-rectangle [state [x y]]
  (if-let [drawing-val (get-in state [:page :drawing])]
    (let [shape-uuid (random-uuid)
          [rect-x rect-y rect-width rect-height] (geo/coords->rect x y (:x drawing-val) (:y drawing-val))
          shape-val (new-rectangle rect-x rect-y rect-width rect-height)]

      (do (pubsub/publish! [:insert-shape [shape-uuid shape-val]])
          (-> state
              (assoc-in [:page :drawing] nil)
              (assoc-in [:page :selected] shape-uuid)
              (assoc-in [:workspace :selected-tool] nil))))

   (assoc-in state [:page :drawing] (map->Rectangle {:x x :y y}))))

(reader/register-tag-parser! (clojure.string/replace (pr-str uxbox.shapes.rectangle/Rectangle) "/" ".")
                             uxbox.shapes.rectangle/map->Rectangle)

(pubsub/publish! [:register-shape {:shape Rectangle
                                   :new new-rectangle
                                   :drawing drawing-rectangle
                                   :key :rect
                                   :icon icons/box
                                   :text "Box (Ctrl + B)"
                                   :menu :tools
                                   :order 10}])
