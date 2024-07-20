(ns badspreadsheet.sheet
  (:require
   [badspreadsheet.server :as server]
   [badspreadsheet.components :as bc]
   [badspreadsheet.cells4 :as c]))

(def state (atom {:active    nil
                  :size      20
                  :camera    {:location [0 0]}
                  :waypoints []
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

(defn init!
  []
  (let [state @state]
    [:<>
     [:script (bc/wrap-js-in-content-loaded "initKeyPressListener();")]
     [:script (bc/wrap-js-in-content-loaded (format "initMouseEventsListener(%s);" (:size state)))]
     ;; initial cell render
     [:div#cell-container
      (into [:<>] (for [[_ cell] (:machines @c/cells)]
                    (bc/cell cell state {:init true})))
      (bc/cursor (:cursor state) state)]
     [:div#overlays
      (bc/information-overlay state)
      (bc/button-bar state)]]))

(def server-map
  {:port       port
   :routes-map {["/" :get] (fn [_] {:body (server/page
                                           (conj server/page-head (deref #'grid-style))
                                           [(init!)])})}})

(defn- render-cell
  [cell-id]
  (println "CELL-ID IS: " cell-id)
  (try
    (server/broadcast!
     server-map
     (let [cell (c/get-cell cell-id)]
       [:div#insert-target
        [:<>
         (bc/cell cell @state)
         [:div#insert-target]]]))
    (catch Exception _e nil)))

;; make this more efficient by only sending changes
(defn- render
  [_ _ _ cells-state]
  (println "RENDER RUNS")
  (try
    (server/broadcast!
     server-map
     (into [:<>] (mapv #(bc/cell % @state) (vals (:machines cells-state)))))
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
        (dosync
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
        (dosync
         (swap! state assoc-in [:cursor :size] new-size)
         (server/broadcast!
          server-map
          (bc/cursor (:cursor @state) @state)))))))

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
        cells     (mapv c/get-cell (badspreadsheet.machines/window position w h))]
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

(defmethod server/data-handler :code
  [{:keys [id code]}]
  (c/formula (c/c-get id :position) code))

(defmethod server/data-handler :mouse-event
  [{:keys [location] :as mouse-event}]
  (move-cursor! (rest location))
  )

(defn start!
  []
  (server/serve! (deref #'server-map)))
