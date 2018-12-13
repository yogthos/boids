(ns boids.core
  (:require
    [quil.core :as q]
    [quil.middleware :as m]
    [reagent.core :as r]))

(def PIx2 (* 2 js/Math.PI))

(defn length [[a b]]
  (js/Math.sqrt (+ (* a a) (* b b))))

(defn average [xs]
  (/ (reduce + xs) (count xs)))

(defn boid-distance [p1 p2]
  (length (mapv (fn [x y] (js/Math.abs (- x y))) p1 p2)))

(defn average-position [boids]
  (->> (map :position boids)
       (apply map vector)
       (map average)))

(defn average-heading [boids]
  (average (map :direction boids)))

(defn heading->angle [[x y]]
  (js/Math.atan2 y x))

(defn angle->heading [angle]
  ((juxt js/Math.cos js/Math.sin) angle))

(defn direction [radians]
  (if (< radians 0)
    (recur (+ PIx2 radians))
    (mod radians PIx2)))

(defn update-direction
  "update the heading of the boid based on its relationship with the neighbours"
  [current-direction p1 p2 s1 s2]
  (direction
    (if (< (heading->angle p1) (heading->angle p2))
      (s1 current-direction 0.1)
      (s2 current-direction 0.1))))

(defn cohere
  "update direction towards the neighbours"
  [{position :position :as boid} neighbour-position]
  (update boid :direction update-direction position neighbour-position + -))

(defn avoid
  "update direction away from the neighbours"
  [{position :position :as boid} neighbour-position]
  (update boid :direction update-direction position neighbour-position - +))

(defn align
  "align with the average heading of the neighbours"
  [{:keys [direction] :as boid} boids-in-range]
  (if (empty? boids-in-range)
    boid
    (let [avg  (average-heading boids-in-range)
          diff (Math/abs (- direction avg))]
      (update boid :direction (if (> direction avg) - +) (/ diff 2)))))

(defn neighbors
  "find neighbours within the given distance threshold"
  [{boid-position :position :as boid} boids threshold]
  (not-empty
    (keep
      (fn [{position :position :as neighbour}]
        (let [distance (boid-distance boid-position position)]
          (when (and (not= boid neighbour) (< distance threshold))
            (assoc neighbour :distance distance))))
      boids)))

(defn boid-direction
  "update the direction of the boid based on its relationship with the neighbours"
  [{:keys [boids cohesion alignment avoidance vision align? cohere? avoid?]}
   {:keys [position] :as boid}]
  (let [boids-in-range             (neighbors boid boids vision)
        boids-to-avoid             (neighbors boid boids-in-range avoidance)
        boids-to-align             (neighbors boid boids-in-range alignment)
        average-neighbour-position (average-position boids-in-range)]
    (cond
      ; avoid the neighbours if the boid is too crowded
      (and avoid? boids-to-avoid)
      (->> (neighbors boid boids-to-avoid avoidance)
           (average-position)
           (avoid boid))
      ; follow the neighbours if there any within cohesion radius
      (and cohere? boids-in-range (> (boid-distance position average-neighbour-position) cohesion))
      (cohere boid average-neighbour-position)
      ; align with the neighbours within the alignment radius
      (and align? boids-to-align)
      (align boid boids-to-align)
      ; retain existing course
      :else boid)))

(defn next-position [[x y] {speed :speed direction :direction}]
  [(+ x (* speed (js/Math.cos direction)))
   (+ y (* speed (js/Math.sin direction)))])

(defn move-boid [boid]
  (update boid :position next-position boid))

(defn wrap [coord bound]
  (if (pos? coord)
    (mod coord bound)
    (+ bound coord)))

(defn wrap-around [{:keys [width height]} boids]
  (map
    (fn [boid]
      (update boid :position #(map wrap % [width height])))
    boids))

(defn move-boids [{:keys [boids] :as state}]
  (->> boids
       (map (partial boid-direction state))
       (map move-boid)
       (wrap-around state)))

(defn gen-boid [radius width height]
  (fn []
    {:position  [(+ radius (rand-int (- width (* 2 radius))))
                 (+ radius (rand-int (- height (* 2 radius))))]
     :direction (rand PIx2)
     :speed     3
     :radius    radius
     :color     [(rand-int 250) (rand-int 250) (rand-int 250)]}))

(defn initial-state [num-boids radius width height]
  (fn []
    {:boids     (repeatedly num-boids (gen-boid radius width height))
     :width     width
     :height    height
     :max-speed 3.5}))

;; -------------------------
;; UI

(def config
  (r/atom
    {:cohesion  50
     :avoidance 20
     :alignment 100
     :vision    100
     :cohere?   true
     :avoid?    true
     :align?    true}))

(defn update-state [state]
  (let [updated-state (move-boids state)]
    (-> (merge state @config)
        (assoc :boids updated-state)
        (update :history #(take 5 (cons updated-state %))))))

(defn draw-boid [{[x y] :position direction :direction [r g b] :color}]
  (q/stroke r g b)
  (let [[dx dy] (angle->heading direction)]
    (q/line x y (+ x (* 5 dx)) (+ y (* 5 dy)))))

(defn draw [{:keys [boids history]}]
  (q/background 255)
  (doseq [boids history]
    (doseq [boid boids]
      (draw-boid boid)))
  (doseq [boid boids]
    (draw-boid boid)))

(defn canvas []
  (r/create-class
    {:component-did-mount
     (fn [component]
       (let [node   (r/dom-node component)
             width  (.-width node)
             height (.-height node)]
         (q/sketch
           :host node
           :draw draw
           :setup (initial-state 25 5 width height)
           :update update-state
           :size [width height]
           :middleware [m/fun-mode])))
     :render
     (fn []
       [:canvas
        {:width  (/ (.-innerWidth js/window) 2)
         :height (/ (.-innerHeight js/window) 2)}])}))

(defn slider [label k]
  [:div
   [:label label " " (str (k @config))]
   [:div>input
    {:type      :range :min 1 :max 100
     :on-change #(swap! config assoc k (js/parseInt (-> % .-target .-value)))
     :value     (k @config)}]])

(defn rule-button [k]
  [:button
   {:on-click #(swap! config update k not)}
   (if (k @config) "enabled" "disabled")])

(defn page []
  [:div
   [:h3 "boid flock"]
   [:table>tbody
    [:tr
     [:td [slider "alignment:" :alignment]]
     [:td [rule-button :align?]]]
    [:tr
     [:td [slider "cohesion:" :cohesion]]
     [:td [rule-button :cohere?]]]
    [:tr
     [:td [slider "avoidance:" :avoidance]]
     [:td [rule-button :avoid?]]]]
   [canvas]])

(defn mount-root []
  (r/render [page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
