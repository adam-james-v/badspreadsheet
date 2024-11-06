(ns badspreadsheet.sheet
  (:require
   [badspreadsheet.cell :as c]
   [badspreadsheet.components :as bc]
   [badspreadsheet.machines :as machines]
   [badspreadsheet.server :as server]
   [badspreadsheet.util :as u]
   [clojure.core.async :as async]
   [clojure.data :as data]
   [clojure.edn :as edn]
   [clojure.string :as str]))

(defonce state (atom {:active    nil
                      :store     {:container-coords [0 0]}
                      :size      20
                      :camera    {:location [0 0]}
                      :waypoints {}
                      :cursor    {:location  [0 0]
                                  :size      [3 3]
                                  :primary   [0 0]
                                  :secondary [0 0]}}))

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
    #_[:clipPath#gridSquareClipPath
     [:path {:d            "M 20 0 L 0 0 0 20"}]]
    [:pattern#gridPattern
     {:width        20
      :height       20
      :patternUnits "userSpaceOnUse"}
     [:path {:d            "M 20 0 L 0 0 0 20"
             :fill         "none"
             :stroke       "#C0B6D0"
             #_#_:clip-path "url(#gridSquareClipPath)"
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
    (let [grouped-cells (group-by :display-hint (sort-by :position (vals (:machines @c/cells))))
          sorted-cells  (apply concat
                               (conj (vec (vals (dissoc grouped-cells :points-editor))) (:points-editor grouped-cells)))]
      (for [cell sorted-cells]
        (bc/cell cell state {:init true}))))
   (bc/cursor (:cursor state) state)
   bc/origin
   (bc/waypoints state)
   (bc/position-refs state @c/cells)
   [:spreadsheet-cursor#sc]])

(defn init-sheet!
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
      (bc/button-bar)
      (bc/arrange-bar state)
      bc/position-indicators
      (let [cell (c/get-cell (get-in state [:view-pane :cell]))]
        (bc/view-pane cell state))]]))

(defn init-focus!
  []
  (let [state @state]
    [:<>
     #_[:script (bc/wrap-js-in-content-loaded "initKeyPressListener();")]
     #_[:script (bc/wrap-js-in-content-loaded (format "initMouseEventsListener(%s);" (:size state)))]
     ;; initial cell render
     [:div#grid-container
      {:style {:display  "inline-block"
               :width    "100vw"
               :height   "100vh"
               :overflow "hidden"
               :position "relative"}}
      grid-square]
     (bc/cursor (:cursor state) state)
     [:div#overlays
      (bc/information-overlay state)
      (bc/button-bar)
      (bc/arrange-bar state)]]))

(defn init!
  []
  (let [{:keys [mode] :or {mode :sheet}} @state]
    [:div#main
     (case mode
       :sheet (init-sheet!)
       :focus (init-focus!))]))

(def server-map
  {:port       port
   :routes-map {["/" :get] (fn [_] {:body (server/page
                                           (conj server/page-head (deref #'grid-style))
                                           [(init!)])})}})

(defn render-cell
  [cell-id]
  (try
    (server/broadcast!
     server-map
     (let [cell (c/get-cell cell-id)]
       [:div#insert-target
        {:hx-swap-oob "afterend"}
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

(defn- direct-render-changes
  [_k _atom old cells-state]
  (let [[_ changes _] (data/diff old cells-state)
        state @state]
    (when changes
      (try
        (server/broadcast!
         server-map
         ;; note that state here is NOT the cell state, but the sheet state
         (let [changed-ids (keys (:machines changes))
               r           (concat
                            (mapv (fn [id]
                                    (let [{:keys [id] :as cell} (get-in cells-state [:machines id])]
                                      (when-not (= id (get-in state [:store :active-element]))
                                        (bc/cell cell state))))
                                  changed-ids)
                            [(bc/position-refs state cells-state)]
                            [(let [cell (c/get-cell (get-in state [:view-pane :cell]))]
                               (bc/view-pane cell state))])]
           r))
        (catch Exception _e nil)))))

;; BASIC AGENT BASED RENDERING

(def pending-changes (agent #{}))
(def render-scheduled (atom false))
#_
(defn- render-accumulated-changes
  [changed-ids]
  (try
    (server/broadcast!
     server-map
     (let [state @state
           r (concat
              (mapv (fn [id]
                      (let [{:keys [id] :as cell} (get-in @c/cells [:machines id])]
                        (when-not (= id (get-in state [:store :active-element]))
                          (bc/cell cell state))))
                    changed-ids)
              [(bc/position-refs state @c/cells)]
              [(let [cell (c/get-cell (get-in state [:view-pane :cell]))]
                 (bc/view-pane cell state))])]
       r))
    (catch Exception _e nil))
  #{}) ; Return empty set to reset pending-changes

(defn- render-accumulated-changes
  [changed-ids]
  (try
    (let [state @state
          cells @c/cells
          active-element (get-in state [:store :active-element])
          view-pane-cell (c/get-cell (get-in state [:view-pane :cell]))]
      (server/broadcast!
       server-map
       (concat
        (into []
              (comp
               (map #(get-in cells [:machines %]))
               (remove #(= (:id %) active-element))
               (map #(bc/cell % state))
               (remove nil?))
              changed-ids)
        [(bc/position-refs state cells)
         (bc/view-pane view-pane-cell state)])))
    (catch Exception e
      (println "Error in render-accumulated-changes:" (.getMessage e))))
  #{}) ; Return empty set to reset pending-changes

(defn- schedule-render []
  (when (compare-and-set! render-scheduled false true)
    (send-off pending-changes
              (fn [changes]
                (Thread/sleep 24)
                (reset! render-scheduled false)
                (render-accumulated-changes changes)))))

(defn- render
  [_k _atom old cells-state]
  (let [[_ changes _] (data/diff old cells-state)]
    (when changes
      (let [changed-ids (remove (set (:active @state)) (keys (:machines changes)))]
        (send pending-changes into changed-ids)
        (schedule-render)))))

(add-watch machines/state :render #'render)















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
  [{:keys [id code]}]
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

(defn- toggle-view-pane
  []
  (swap! state update-in [:view-pane :active] not)
  (let [cell (c/get-cell (get-in @state [:view-pane :cell]))]
    (server/broadcast!
     server-map
     (bc/view-pane cell @state))))

(defn- toggle-pin-movement
  []
  (swap! state update-in [:store :pin-movement] not)
  (render-cursor))

(defn- set-view-pane
  [id-or-pos]
  (let [id-or-pos (try
                    (edn/read-string id-or-pos)
                    (catch Exception _ nil))]
    (when id-or-pos
      (swap! state assoc-in [:view-pane :cell] id-or-pos)
      (let [cell (c/get-cell id-or-pos)]
        (server/broadcast!
         server-map
         (bc/view-pane cell @state))))))

(defn arrange-h!
  ([]
   (let [{:keys [primary secondary]} (:cursor @state)]
     (arrange-h! primary secondary)))
  ([corner1 corner2]
   (let [[w h]       [6 6]
         cell-ids    (machines/get-ids-at-positions @c/cells (u/window corner1 corner2))
         cells       (sort-by :position (distinct (keep #(c/get-cell %) cell-ids)))
         [x1 y1]     (:position (first cells))
         new-cells   (into {}
                           (map-indexed (fn [idx {:keys [id]}]
                                          [id {:position [(+ x1 (* idx (inc w))) y1]
                                               :size     [w h]}])
                                        cells))
         bulk-update (fn bulk-update
                       [state]
                       (if (seq new-cells)
                         (-> state
                             (update :machines (partial merge-with merge) new-cells)
                             machines/update-grid)
                         state))]
     (swap! c/cells bulk-update)
     true)))

(defn arrange-v!
  ([]
   (let [{:keys [primary secondary]} (:cursor @state)]
     (arrange-v! primary secondary)))
  ([corner1 corner2]
   (let [[w h]       [6 6]
         cell-ids    (machines/get-ids-at-positions @c/cells (u/window corner1 corner2))
         cells       (sort-by :position (distinct (keep #(c/get-cell %) cell-ids)))
         [x1 y1]     (:position (first cells))
         new-cells   (into {}
                           (map-indexed (fn [idx {:keys [id]}]
                                          [id {:position [x1 (+ y1 (* idx (inc h)))]
                                               :size     [w h]}])
                                        cells))
         bulk-update (fn bulk-update
                       [state]
                       (if (seq new-cells)
                         (-> state
                             (update :machines (partial merge-with merge) new-cells)
                             machines/update-grid)
                         state))]
     (swap! c/cells bulk-update)
     true)))

(defn stack!
  ([]
   (let [{:keys [primary secondary]} (:cursor @state)]
     (stack! primary secondary)))
  ([corner1 corner2]
   (let [cell-ids      (machines/get-ids-at-positions @c/cells (u/window corner1 corner2))
         cells         (sort-by :position (distinct (keep #(c/get-cell %) cell-ids)))
         [x1 y1]       (:position (first cells))
         size          (:size (first cells))
         grouped-cells (group-by :display-hint cells)
         sorted-cells  (apply concat
                              (conj (vec (vals (dissoc grouped-cells :points-editor))) (:points-editor grouped-cells)))
         new-cells     (into {}
                             (map (fn [{:keys [id]}]
                                    [id {:position [x1 y1]
                                         :size     size}])
                                sorted-cells))
         bulk-update   (fn bulk-update
                       [state]
                       (if (seq new-cells)
                         (-> state
                             (update :machines (partial merge-with merge) new-cells)
                             machines/update-grid)
                         state))]
     (swap! c/cells bulk-update)
     true)))

(defmethod server/data-handler :run-command
  [{:keys [command args]}]
  (let [cmds {:process-one         machines/process-one!
              :toggle-view-pane    toggle-view-pane
              :toggle-pin-movement toggle-pin-movement
              :set-view-pane-cell  set-view-pane
              :stack               stack!
              :arrange-v           arrange-v!
              :arrange-h           arrange-h!}
        args (rest args)
        cmd  (get cmds (keyword command) (fn [& _args]
                                           (println "command not found: " command)))]
    (apply cmd args)))

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
     [:<>
      (bc/waypoints @state)
      (bc/arrange-bar @state)])))

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
      (let [new-display (if display-hint
                          (next-display display)
                          ({:editor :value
                            :value  :editor} display))]
        (when new-display
          (c/c-assoc id :display new-display))))))

(defmethod server/data-handler :toggle-mode
  [_]
  (swap! state update :mode #(get {:sheet :focus
                                   :focus :sheet} % :sheet))
  (server/broadcast!
   server-map
   (init!)))

(defn start!
  []
  (server/serve! (deref #'server-map)))
