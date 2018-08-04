(ns boids.core
  (:require
    [cljsjs.pixi]
    [reagent.core :as r]
    [re-frame.core :as rf]))

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

(defn move-boids [state]
  (update state :boids
          #(->> %
                (map (partial boid-direction state))
                (map move-boid)
                (wrap-around state))))

(defn gen-boid [radius width height]
  (fn []
    {:position  [(+ radius (rand-int (- width (* 2 radius))))
                 (+ radius (rand-int (- height (* 2 radius))))]
     :direction (rand PIx2)
     :speed     3
     :radius    radius
     :color     (repeatedly 3 #(max 10 (rand-int 100)))
     :graphics  (js/PIXI.Graphics.)}))

(rf/reg-event-db
  :init-game
  (fn [db [_ num-boids radius width height stage]]
    (-> db
        (assoc :stage stage)
        (update :state merge {:boids     (repeatedly num-boids (gen-boid radius width height))
                              :width     width
                              :height    height
                              :max-speed 3.5}))))

(rf/reg-event-db
  :init-config
  (fn [_ _]
    {:state {:cohesion  50
             :avoidance 20
             :alignment 100
             :vision    100
             :cohere?   true
             :avoid?    true
             :align?    true}}))

;; -------------------------
;; UI

(rf/reg-event-db
  :update-state
  (fn [db _]
    (update db :state move-boids)))

;;;
(rf/reg-event-db
  :set-stage
  (fn [db [_ stage]]
    (assoc db :stage stage)))

(rf/reg-sub
  :stage
  (fn [db _]
    (:stage db)))

(rf/reg-event-db
  :set-state
  (fn [db [_ state]]
    (assoc db :state state)))

(rf/reg-sub
  :state
  (fn [db _]
    (:state db)))

(defn draw-boid [{graphics :graphics color :color [x y] :position direction :direction}]
  (let [[dx dy] (angle->heading direction)]
    (-> graphics
        (.clear)
        (.lineStyle 5, (js/PIXI.utils.rgb2hex (clj->js color)), 1)
        (.moveTo x y)
        (.lineTo (+ x dx) (- y dy))
        (.endFill))))

(defn draw [{:keys [boids]}]
  (doseq [boid boids]
    (draw-boid boid)))

(defn ticker []
  (draw @(rf/subscribe [:state]))
  (rf/dispatch [:update-state]))

(defn add-to-stage [app child]
  (.addChild (goog.object/get app "stage") child))

(defn init-canvas [dom-node width height ticker]
  (let [app (js/PIXI.Application. width height #js {:antialias       true
                                                    :backgroundColor 0xFFFFFF})]
    (rf/dispatch-sync [:init-game 50 3 width height {:app app}])
    (.appendChild dom-node (.-view app))
    (doseq [{:keys [graphics]} (:boids @(rf/subscribe [:state]))]
      (add-to-stage app graphics))
    (.add (.-ticker app) ticker)
    (.start app)))

(defn canvas []
  (r/create-class
    {:component-did-mount
     (fn [component]
       (let [node (r/dom-node component)]
         (init-canvas node 500 500 ticker)))
     :render
     (fn [] [:div])}))

;;;
(rf/reg-event-db
  :set-config-key
  (fn [db [_ k v]]
    (assoc-in db [:state k] v)))

(rf/reg-event-db
  :update-config-key
  (fn [db [_ k f]]
    (update-in db [:state k] f)))

(rf/reg-sub
  :config-key
  (fn [db [_ k]]
    (get-in db [:state k])))

(rf/reg-sub
  :db (fn [db _] db))

(defn slider [label k]
  [:div
   [:label label " " (str @(rf/subscribe [:config-key k]))]
   [:div>input
    {:type      :range :min 1 :max 100
     :on-change #(rf/dispatch [:set-config-key k (js/parseInt (-> % .-target .-value))])
     :value     @(rf/subscribe [:config-key k])}]])

(defn rule-button [k]
  [:button
   {:on-click #(rf/dispatch [:update-config-key k not])}
   (if @(rf/subscribe [:config-key k]) "enabled" "disabled")])

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
  (rf/dispatch-sync [:init-config])
  (mount-root))
