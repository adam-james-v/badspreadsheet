(ns badspreadsheet.sheet
  (:require
   [badspreadsheet.machines :as machines]
   [badspreadsheet.cell :as c]
   [badspreadsheet.components :as bc]
   [badspreadsheet.server :as server]
   [badspreadsheet.util :as u]
   [clojure.string :as str]))

(defonce state (atom {:active    nil
                      :store     {:container-coords [0 0]}
                      :size      20
                      :camera    {:location [0 0]}
                      :waypoints {}
                      :cursor    {:location [0 0]
                                  :size     [3 3]}}))

(defonce port (server/get-port {:port (range 8000 9000)}))

(def grid-style
  [:style
   [:hiccup/raw-html
    (str "
body {
  font-family: 'Berkeley Mono', monospace;
  font-size: 10.5pt;
  overflow: hidden;
}
")]])

(def grid-square
  [:svg#grid
   {:width  "100%"
    :height "100%"
    :style  {:position "absolute"
             :top      0
             :left     0
             :width    "100%"
             :height   "100%"}}
   [:defs
    [:pattern#gridPattern
     {:width        20
      :height       20
      :patternUnits "userSpaceOnUse"}
     [:path {:d            "M 20 0 L 0 0 0 20"
             :fill         "none"
             :stroke       "#C0B6D0"
             :stroke-width 1}]]]
   [:rect {:width  "100%"
           :height "100%"
           :fill   "#E6E6FA"}]
   [:rect {:width  "100%"
           :height "100%"
           :fill   "url(#gridPattern)"}]])

(defn cell-container
  [state]
  [:div#cell-container
   {:style
    {:position  "absolute"
     :top       0
     :transform (let [[x y] (get-in state [:store :container-coords] [0 0])]
                  (format "translate(%spx, %spx);" x y))}}
   (into
    [:<>]
    (for [[_ cell] (:machines @c/cells)]
      (bc/cell cell state {:init true})))
   (bc/cursor (:cursor state) state)
   bc/origin
   (bc/waypoints state)
   (bc/position-refs state @c/cells)
   [:spreadsheet-cursor#sc]])

(defn init!
  []
  (let [state @state]
    [:<>
     [:script (bc/wrap-js-in-content-loaded "initKeyPressListener();")]
     [:script (bc/wrap-js-in-content-loaded (format "initMouseEventsListener(%s);" (:size state)))]
     ;; initial cell render
     [:div#grid-container
      {:style {:display  "inline-block"
               :width    "100vw"
               :height   "100vh"
               :overflow "hidden"
               :position "relative"}}
      grid-square]
     (cell-container state)
     [:div#overlays
      (bc/information-overlay state)
      (bc/button-bar state)
      [:div#row-indicators]
      [:div#column-indicators]
      [:div
       {:style {:position         "fixed"
                :width            60
                :height           60
                :top              0
                :left             0
                :background-color "#E6E6FA"
                :border-right     "1px solid #C0B6D0"
                :border-bottom    "1px solid #C0B6D0"}}]]]))

(def server-map
  {:port       port
   :routes-map {["/" :get] (fn [_] {:body (server/page
                                           (conj server/page-head (deref #'grid-style))
                                           [(init!)])})}})

(defn- render-cell
  [cell-id]
  (try
    (server/broadcast!
     server-map
     (let [cell (c/get-cell cell-id)]
       [:div#insert-target {:hx-swap-oob "afterend"}
        (bc/cell cell @state)]))
    (catch Exception _e nil)))

(defn- render-cursor
  []
  (try
    (server/broadcast!
     server-map
     (let [state @state]
       (bc/cursor (:cursor state) state)))
    (catch Exception _e nil)))

;; make this more efficient by only sending changes
(defn- render
  [_k _atom _old cells-state]
  (try
    (server/broadcast!
     server-map
     ;; note that state here is NOT the cell state, but the sheet state
     (let [r (concat
              (mapv (fn [{:keys [id] :as cell}]
                       (when-not (= id (get-in @state [:store :active-element]))
                        (bc/cell cell @state)))
                    (vals (:machines cells-state)))
              [(bc/position-refs @state cells-state)])]
       r))
    (catch Exception _e nil)))

(add-watch c/cells :render #'render)

(defn- move-cursor!
  [direction-or-location]
  (let [{kursor :cursor}        @state
        {:keys [location size]} kursor]
    (when location
      (let [new-loc    (if (keyword? direction-or-location)
                         (mapv + location ({:left  [-1 0]
                                            :right [1 0]
                                            :up    [0 -1]
                                            :down  [0 1]} direction-or-location))
                         direction-or-location)
            new-cursor {:location new-loc
                        :size     size}]
        #_(dosync
         (swap! state assoc
                :cursor new-cursor
                :active nil)
         (server/broadcast! server-map (bc/cursor (:cursor @state) @state)))))))

(defn- resize-cursor!
  [direction-or-size]
  (let [direction?       (keyword? direction-or-size)
        {kursor :cursor} @state
        {:keys [size]}   kursor
        [x y :as size]   (if (every? number? size)
                           size
                           [3 3])
        direction        (when direction?
                           (case direction-or-size
                             :left  :h-
                             :right :h+
                             :up    :v-
                             :down  :v+))]
    (when size
      (let [new-size (if direction
                       (mapv + size ({:h- [-1 0] :h+ [1 0] :v- [0 -1] :v+ [0 1]} direction))
                       direction-or-size)]
        #_(when (every? #(> % 0) new-size))
        #_(dosync
         (swap! state assoc-in [:cursor :size] new-size)
         (server/broadcast!
          server-map
          (bc/cursor (:cursor @state) @state)))))))

(defn- adjust-cursor!
  [{:keys [location size primary secondary]}]
  (dosync
   (swap! state update :cursor merge {:location location :size size :primary primary :secondary secondary})
   (server/broadcast!
    server-map
    (bc/cursor (:cursor @state) @state))))

(defn- add-cell
  []
  (let [{position :location
         size     :size} (get @state :cursor)]
    (-> (c/formula position size "")
        render-cell)))

(defmethod server/data-handler :keypress
  [{keys-pressed :keys}]
  (case (vec (rest keys-pressed))
    ["enter"] (add-cell)

    ["left"]  (move-cursor! :left)
    ["right"] (move-cursor! :right)
    ["up"]    (move-cursor! :up)
    ["down"]  (move-cursor! :down)

    ["shift" "left"]  (resize-cursor! :left)
    ["shift" "right"] (resize-cursor! :right)
    ["shift" "up"]    (resize-cursor! :up)
    ["shift" "down"]  (resize-cursor! :down)
    ;; default
    (println "UNHANDLED KEYS: " keys-pressed)))

(defmethod server/data-handler :add-cell
  [_]
  (add-cell))

(defmethod server/data-handler :remove-cell
  [_]
  (let [position (vec (get-in @state [:cursor :location]))
        [w h]    (vec (get-in @state [:cursor :size]))
        cells    (distinct (mapv c/get-cell (u/window position w h)))]
    (c/remove! (mapv :id cells))
    (server/broadcast!
     server-map
     (map
      (fn [cell]
        [:div {:id          (format "entity%s" (:id cell))
               :hx-swap-oob "outerHTML"}])
      cells))))

(defmethod server/data-handler :move-cursor
  [{:keys [position]}]
  (let [[_ x y] position]
    (move-cursor! [x y])))

(defmethod server/data-handler :adjust-cursor
  [{:keys [size location primary secondary]}]
  (let [[_ w h]   size
        [_ x y]   location
        [_ px py] primary
        [_ sx sy] secondary]
    (adjust-cursor! {:size      [w h]
                     :location  [x y]
                     :primary   [px py]
                     :secondary [sx sy]})))

(defmethod server/data-handler :code
  [{:keys [id code] :as a}]
  (def asdf a)
  (c/update-formula id code))

(defmethod server/data-handler :mouse-event
  [{:keys [location]}]
  #_(move-cursor! (rest location)))

(defmethod server/data-handler :store
  [data]
  (let [data (->> (dissoc data :dispatch)
                  (mapv (fn [[k v]]
                          (if (vector? v)
                            [k (vec (remove #(= (name k) %) v))]
                            [k v])))
                  (into {}))]
    (swap! state update :store merge data)
    (render-cursor)))

(defmethod server/data-handler :run-command
  [{:keys [command args]}]
  (let [cmds {:process-one machines/process-one!}]
    (apply (get cmds (keyword command)) args)))

(defmethod server/data-handler :adjust-cell
  [{:keys [id x y w h]}]
  (let [id (parse-long (str/replace id "movable" ""))]
    (when id
      (c/c-merge id {:position [x y]
                     :size     [w h]}))))

(defmethod server/data-handler :toggle-waypoint
  [_]
  (let [position (vec (get-in @state [:cursor :location]))]
    (swap! state update :waypoints
           (fn [ws]
             (if (contains? ws position)
               (dissoc ws position)
               (assoc ws position {:label  ""
                                   :colour (bc/random-colour)}))))
    (server/broadcast!
     server-map
     (bc/waypoints @state))))

(def next-display
  {:editor :control
   :control :value
   :value :editor})

(defmethod server/data-handler :toggle-display
  [{:keys [id]}]
  (let [position (vec (get-in @state [:cursor :location]))
        [w h]    (vec (get-in @state [:cursor :size]))
        cells    (remove nil? (distinct (mapv c/get-cell (if id
                                                           [id]
                                                           (u/window position w h)))))]
    ;; todo: create a bulk op here to only swap the c/cells once
    (doseq [{:keys [id display display-hint]} cells]
      (c/c-assoc id :display (or (if display-hint
                                   (next-display display)
                                   ({:editor :value
                                     :value  :editor} display)) :value)))))

(defn start!
  []
  (server/serve! (deref #'server-map)))
