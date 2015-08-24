(ns uxbox.shapes.group
  (:require [uxbox.shapes.core :as s]
            [uxbox.pubsub :as pubsub]
            [uxbox.icons :as icons]
            [uxbox.geometry :as geo]
            [uxbox.icons :as icons]
            [cljs.reader :as reader]
            [reagent.core :refer [atom]]))

(def group-menu {:name "Size and position"
                     :icon icons/infocard
                     :key :options
                     :options [{:name "Position" :inputs [{:name "X" :type :number :shape-key :x :value-filter int}
                                                          {:name "Y" :type :number :shape-key :y :value-filter int}]}]})

(defrecord Group [x y rotate shapes]
  s/Shape

  (intersect [{:keys [shapes]} px py]
    (apply some (map s/intersect shapes)))

  (toolbar-coords [{:keys [x y shapes]}]
    (let [vx (+ x 50)
          vy (- y 50)]
      (geo/viewportcord->clientcoord vx vy)))

  (shape->svg [{:keys [x y rotate shapes]}]
    [:g
     {:x x
      :y y
      :rotate rotate
      :transform (s/generate-transformation {:rotate rotate})}
     (map s/shape->svg shapes)])

  (shape->selected-svg [{:keys [x y rotate]}]
    [:g
     [:rect {:x (- x 4)
             :y (- y 4)
             :rotate rotate
             :transform (s/generate-transformation {:rotate rotate :center {:x (+ x (/ width 2)) :y (+ y (/ height 2))}})}]

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
    [group-menu s/actions-menu][])
  )

(defn new-group
  "Retrieves a line with the default parameters"
  [x y shapes]
  (Group. x y 0 shapes))

(defn drawing-group [state [x y]]
 (if-let [drawing-val (get-in state [:page :drawing])]
   (let [shape-uuid (random-uuid)
         [rect-x rect-y rect-width rect-height] (geo/coords->rect x y (:x drawing-val) (:y drawing-val))
         shape-val (new-group rect-x rect-y [])]

     (do (pubsub/publish! [:insert-shape [shape-uuid shape-val]])
         (-> state
              (assoc-in [:page :drawing] nil)
              (assoc-in [:page :selected] shape-uuid)
              (assoc-in [:workspace :selected-tool] nil))))

   (assoc-in state [:page :drawing] (map->Group {:x x :y y}))))

(reader/register-tag-parser! (clojure.string/replace (pr-str uxbox.shapes.group/Group) "/" ".") uxbox.shapes.group/map->Group)
