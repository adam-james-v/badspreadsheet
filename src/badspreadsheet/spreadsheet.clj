(ns badspreadsheet.spreadsheet
  (:require
   [badspreadsheet.cells3 :as c]
   [badspreadsheet.components :as bc]
   [badspreadsheet.server :as server]
   [clojure.core.async :as a
    :refer [chan go-loop
            pub sub unsub
            >! <! >!! <!!]]
   [clojure.data :as data]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [overtone.at-at :as at]
   [squint.compiler]))

(def state-map
  {:size      20
   :active    nil
   :camera    {:location [0 0]}
   :cursor    {:location [0 0] :size [7 3]}
   :entities  {}
   :timers    {}
   :waypoints {}
   :extents   [100 100]})

(defonce state
  (atom state-map))

(defonce location-index
  (atom {}))

;; map of locations to the cells looking at that location
(defonce location-registry
  (atom []))

(defn- covers
  [{:keys [location size]}]
  (let [[x y]   location
        [nx ny] size]
    (for [sx (range nx)
          sy (range ny)]
      [(+ x sx) (+ y sy)])))

(defn- entity-covers
  [entity]
  (into #{} (covers entity)))

(defn dissoc-val
  [m value]
  (into {} (remove (fn [[_ v]] (= v value))) m))

(defn untrack-location!
  [{:keys [id]}]
  (let [xf (fn [m]
             (-> m
                 (dissoc-val id)))]
    (swap! location-index xf)))

(defn track-location!
  [{:keys [id] :as entity}]
  (let [xf (fn [m]
             (-> m
                 (dissoc-val id)
                 (merge (zipmap (covers entity) (repeat id)))))]
    (c/touch! c/location-touch)
    (swap! location-index xf)))

(defonce ^:private entity-counter (atom -1))
;; threadpool and timing stuff
(defonce runner-pool (at/mk-pool))

(defn clear-state! []
  (c/destroy-cells!)
  (reset! c/global-cells {})
  (reset! entity-counter -1)
  (at/stop-and-reset-pool! runner-pool)
  (reset! state state-map))

(def grid-style
  [:style
   [:hiccup/raw-html
    (str "
.fade-in.htmx-added {
  opacity: 0;
}
.fade-in {
  opacity: 1;
  transition: opacity 1s ease-out;
}
.bg-grid {
  background-repeat: repeat;
  background-image: url(\"data:image/svg+xml,%3Csvg xmlns='http://www.w3.org/2000/svg' width='" (:size @state) "' height='" (:size @state) "'%3E%3Crect width='100%' height='100%' fill='%23736372' /%3E%3Crect x='0.5%' y='0.5%' width='99%' height='99%' fill='%23D1CFE2' /%3E%3C/svg%3E%0A\");
}

body {
  font-family: 'Berkeley Mono', monospace;
  font-size: 10.5pt;
}
")]])

(declare init-grid-for-get!)

(defonce port (server/get-port {:port (range 8000 9000)}))

(def server-map
  {:port       port
   :routes-map {["/" :get] (fn [_] {:body (server/page
                                           (conj server/page-head (deref #'grid-style))
                                           [bc/points-editor-template
                                            [:div#bg.bg-grid
                                             {:style {:position "fixed"
                                                      :width    "110vw"
                                                      :height   "110vh"}}
                                             [:div#entity-container
                                              {:style {:width  "110vw"
                                                       :height "110vh"}}
                                              (init-grid-for-get!)]]])})}})

(defn- new-entity-id
  "What will be the next cell's ID? Use that without incrementing the cell counter, because the cell creation does that."
  []
  (inc @c/cell-counter))

(defn broadcast-content-into!
  [id content]
  (server/broadcast!
       server-map
       [:div {:id id} content]))

(defonce previous-render (atom #{}))
(defn bulk-render-and-broadcast []
  (let [values        (->> @c/global-cells
                           vals
                           (mapv (fn [cell]
                                   (let [{:keys [id output error display]} @cell]
                                     [id {:id id
                                          :display display
                                          :value (or error output)}])))
                           (into {}))
        [_ changed _] (data/diff @previous-render values)
        messages      (select-keys values (keys changed))]
    (when (seq messages)
      (reset! previous-render messages)
      (server/broadcast!
       server-map
       (map
          bc/render-value2
          (vals messages))))))

(defn- add-value
  [{:keys [id] :as entity}]
  (let [v (c/value id)]
    (assoc entity :value v)))

(defn bulk-render-and-broadcast2
  []
  (when-let [entities (map add-value (vals (get-in @state [:entities])))]
    (server/broadcast!
     server-map
     (into [:<>] (map bc/render-value2) entities))))

(add-watch c/global-cells :state-render
           (fn [_ _ _ n]
             (when (seq n)
               (bulk-render-and-broadcast2))))

(defonce watcher-control-chan (chan))
(defonce watcher-control-chan-mult (a/mult watcher-control-chan))
(defonce watcher-running (atom false))

(defn stop-watcher []
  (>!! watcher-control-chan true)
  (reset! watcher-running false))

#_
(defn start-watcher []
  (when-not @watcher-running
    (reset! watcher-running true)
    (let [listen-chan (chan 10)
          broadcast-interval-ms 16
          accumulator (atom false)
          control-a (chan)
          control-b (chan)]
      (a/tap c/event-bus-mult listen-chan)
      (a/tap watcher-control-chan-mult control-a)
      (a/tap watcher-control-chan-mult control-b)
      ;; Message listening loop
      (go-loop []
        (let [[_m ch] (a/alts! [listen-chan control-a])]
          (if (= ch listen-chan)
            (do (reset! accumulator true)
                (recur))
            (do (a/untap c/event-bus-mult listen-chan)
                (a/untap watcher-control-chan-mult control-a)
                (println "accumulator loop in watcher stopped")))))
      ;; Broadcast loop
      (go-loop []
        ;; Wait for the next broadcast interval
        (let [[_ ch] (a/alts! [(a/timeout broadcast-interval-ms) control-b])]
          ;; Broadcast accumulated messages
          (if (not= ch control-b)
            (do (when @accumulator
                  (#'bulk-render-and-broadcast2)
                  (reset! accumulator false))
                (recur))
            (do (a/untap watcher-control-chan-mult control-b)
                (println "broadcast loop in watcher stopped"))))))))

#_
(defn start-watcher2 []
  (when-not @watcher-running
    (reset! watcher-running true)
    (let [listen-chan (chan (a/sliding-buffer 1))
          control-a (chan)]
      (a/tap c/event-bus-mult listen-chan)
      (a/tap watcher-control-chan-mult control-a)
      ;; Message listening and broadcast loop
      (go-loop []
        (let [[m ch] (a/alts! [listen-chan control-a])]
          (if (= ch listen-chan)
            (do #_(println "MESSAGE: " m)
                (#'bulk-render-and-broadcast2)
                (recur))
            (do (a/untap c/event-bus-mult listen-chan)
                (a/untap watcher-control-chan-mult control-a)
                (println "loop in watcher stopped"))))))))

(defn re-render!
  [ids]
  (let [entities (:entities @state)]
    (server/broadcast!
     server-map
     (into [:<>] (map #(bc/editor (get entities %) @state)) ids))))

(defn make-entity
  [loc size]
  (let [id   (new-entity-id)
        cell (-> (c/formula (fn [] "")))]
    #_(c/touch! cell)
    (c/c-merge cell {:display  :content
                     :location loc
                     :size     size})
    {:id       id
     :location loc
     :size     size
     :display  :content
     :cell     cell
     :content  ""
     :error    nil
     :timers   nil}))

(defn make-entity!
  ([loc] (make-entity! loc [1 1]))
  ([loc size]
   (let [{:keys [id] :as entity} (make-entity loc size)
         xf                      (fn [state]
                                   (-> state
                                       (assoc :active id)
                                       (assoc-in [:entities id] entity)))]
     (swap! state xf)
     id)))

(defn remove-entity!
  [id]
  (let [lstate                    @state
        {:keys [cell] :as entity} (get-in lstate [:entities id])]
    (when entity
      (let [xf (fn [state]
                           (-> state
                               (update :entities dissoc id)))]
        (swap! state xf)
        (untrack-location! entity)
        (c/destroy! cell)))))

(defn move-entity!
  [new-loc entity-id]
  (let [lstate @state
        entity (get-in lstate [:entities entity-id])]
    (when entity
      (let [moved-entity (assoc entity :location new-loc)
            xf           (fn [state]
                           (-> state
                               (assoc-in [:entities entity-id] moved-entity)))]
        (c/c-assoc entity-id :location new-loc)
        (c/touch! entity-id)
        (track-location! moved-entity)
        (swap! state xf)
        moved-entity))))

(defn move-entity-relative!
  [rel-loc new-loc entity-id]
  (let [lstate @state
        {:keys [location] :as entity} (get-in lstate [:entities entity-id])
        new-loc (mapv + new-loc (mapv - location rel-loc))]
    (when entity
      (let [moved-entity (assoc entity :location new-loc)
            xf           (fn [state]
                           (-> state
                               (assoc-in [:entities entity-id] moved-entity)))]
        (c/c-assoc entity-id :location new-loc)
        (c/touch! entity-id)
        (track-location! moved-entity)
        (swap! state xf)
        moved-entity))))

(defn resize-entity!
  [new-size entity-id]
  (when (every? #(> % 0) new-size)
    (let [lstate @state
          entity (get-in lstate [:entities entity-id])]
      (when entity
        (let [resized-entity (assoc entity :size new-size)
              xf             (fn [state]
                               (-> state
                                   (assoc-in [:entities entity-id] resized-entity)))]
          (c/c-assoc entity-id :size new-size)
          (c/touch! entity-id)
          (track-location! resized-entity)
          (swap! state xf))))))

(def display-sequence
  [:content :note :value :control :none])

(defn cycle-entity-display
  ([entity] (cycle-entity-display entity :up))
  ([entity direction]
   (let [vals         (conj (vec (rest display-sequence)) (first display-sequence))
         next-display (case direction
                        :up   (zipmap display-sequence vals)
                        :down (zipmap vals display-sequence))
         nd (next-display (:display entity))]
     (c/c-assoc (:cell entity) :display nd)
     (update entity :display next-display))))

(defn maybe-read-string [s]
  (try
    ;; Read all forms from the string and wrap them in a `do` block.
    (let [forms (atom [])] ; An atom to collect all forms read from the string.
      (with-open [r (java.io.PushbackReader. (java.io.StringReader. s))]
        (loop []
          (let [form (read r nil ::end)]
            (if (= form ::end)
              @forms
              (do
                (swap! forms conj form)
                (recur))))))
      ;; If successful, wrap the forms in a 'do block.
      (let [read-forms @forms]
        (if (> (count read-forms) 1)
          (cons 'do read-forms)
          (first read-forms))))
    (catch Exception e
      (println "Error reading string."
               s
               (ex-cause e))
      nil)))

(def ^:private sharp-forms
  {'c#   'badspreadsheet.spreadsheet/c#   ;; value by id
   't#   'badspreadsheet.spreadsheet/t#   ;; timer tick every n ms
   'l#   'badspreadsheet.spreadsheet/l#   ;; value by location
   'tap# 'badspreadsheet.spreadsheet/tap# ;; sets up a tap target
   })

(defn collect-sharp-forms
  [form]
  (letfn [(collect [x]
            (cond
              (and (list? x) (seq x) ((set (keys sharp-forms)) (first x))) [x]
              (coll? x) (mapcat collect x)
              :else []))]
    (vec (distinct (collect form)))))

(defn c# [id] id)

(defn swap-zero-arity-formula!
  [cell f & args]
  (let [value (c/value cell)]
    (c/swap-function!
     cell
     (fn [] (apply f value args))))
  (c/value cell))

(defn- make-time-cell!
  [interval]
  (let [cell        (c/formula (fn [] 0))
        updater     (fn [] (swap-zero-arity-formula! cell inc))
        runner      (at/every interval updater runner-pool)
        timer-entry {:cell   cell
                     :runner runner}]
    (swap! state assoc-in [:timers interval] timer-entry)
    cell))

(defn t# [interval]
  (let [{:keys [cell]} (get-in @state [:timers interval])]
    (if cell
      cell
      (make-time-cell! interval))))

(defn l#
  "Location reference."
  [loc]
  loc)

(def core-syms
  (keys (ns-publics (find-ns 'clojure.core))))

(defmacro qualify-user-syms
  [form]
  (let [user-ns      (find-ns 'user)
        public-syms  (ns-publics user-ns)
        aliases      (ns-aliases user-ns)
        refers       (apply dissoc (concat [(ns-refers (find-ns 'user))] core-syms))
        all-syms     (merge public-syms refers)
        qualify-sym  (fn [sym]
                       (let [split-sym (clojure.string/split (str sym) #"/")]
                         (if (= 1 (count split-sym))
                           (if-let [v (get all-syms (symbol sym))]
                             (symbol (str (ns-name (ns v)) "/" sym))
                             sym)
                           (let [alias     (symbol (first split-sym))
                                 actual-ns (get aliases alias)]
                             (if actual-ns
                               (symbol (str (ns-name actual-ns) "/" (second split-sym)))
                               sym)))))
        qualify-form (fn [frm]
                       (walk/postwalk (fn [x]
                                        (if (symbol? x) (qualify-sym x) x))
                                      frm))]
    (qualify-form form)))

(defn formulize
  [form]
  (let [sharps           (collect-sharp-forms form)
        syms             (mapv (fn [[sym id]] (symbol (format "%s%s" sym (str/replace (str id) #"[ \[\]]" "_")))) sharps)
        smap             (zipmap sharps syms)
        input-cell-forms (vec (walk/postwalk-replace sharp-forms sharps))
        bound-fn         `(memoize
                           (fn ~syms
                             (binding [*ns* (find-ns 'user)])
                             ~(walk/postwalk-replace smap (qualify-user-syms form))))
        input-forms      (when (seq input-cell-forms) (map eval input-cell-forms))]
    {:fun         (eval bound-fn)
     :form        form
     :input-cells input-forms}))

(defn maybe-formulize
  [form]
  (try
    (formulize form)
    (catch Exception e
      (println "Error formulizing form."
               form
               (ex-cause e)))))

(defn reset-cell!
  [id form]
  (let [{:keys [fun _form input-cells]} (maybe-formulize form)]
    (when fun
      (c/reset-function! id fun input-cells)
      (c/touch! c/location-touch))))

(defn grid-square
  [x y size]
  [:div.grid-square
   [:div {:style {:border-style   "solid" :border-color "gray" :border-width 1 :box-sizing "border-box"
                  :position       "absolute"
                  :pointer-events "none"
                  :left           (* x size)
                  :top            (* y size)
                  :width          size
                  :height         size}}]])

(defn render-entities
  []
  (into [:<>]
        (for [[_ entity] (:entities @state)]
          (bc/editor entity @state))))

(defn- handle-entity
  ([req] (handle-entity req false))
  ([{:keys [id code]} init?]
   (when id
     (let [code      (if init? "" code) ;; intentionally initialize entities with empty code for loading purposes
           id        (if (string? id) (parse-long id) id)
           prev-str  (:content (get-in @state [:entities id]))
           prev-form (:form (get-in @state [:entities id]))
           new-form  (when (not (= (str/trim code) (str/trim prev-str))) (maybe-read-string code))
           form      (when (not= new-form prev-form) new-form)
           cell-id   (get-in @state [:entities id :cell])]
       (track-location! (get-in @state [:entities id]))
       (when (or
              init?
              (= code "")
              form)
         (swap! state (fn [m]
                        (-> m
                            (assoc-in [:entities id :content] code)
                            (assoc-in [:entities id :form] form))))
         (reset-cell! cell-id form))))))

(defmethod server/data-handler :code
  [req]
  (handle-entity req))

#_
(defn save-entities!
  [fname]
  (let [{:keys [entities waypoints]} @state
        cleaned (mapv (fn [entity] (dissoc entity :cell :watcher)) (vals entities))]
    (spit fname (vec (sort-by :id cleaned)))))

(defn save!
  [fname]
  (spit fname (dissoc @state :timers)))

(defn- insert-entity!
  "Inserts a loaded entity into the app state."
  [{:keys [content id location size] :as entity}]
  (make-entity! location size)
  (swap! state update-in [:entities id] merge entity)
  (handle-entity {:code content :id id} true))

(defn- copy-entities!
  []
  (let [{entities :entities occupied :occupied kursor :cursor} @state
        entities                                               (vals entities)
        in-cursor                                              (entity-covers kursor)]
    (when (some in-cursor occupied)
      (let [to-copy (filter (fn [{:keys [location size]}]
                              (and (in-cursor location)
                                   (in-cursor (mapv + location (map dec size))))) entities)]
        (swap! state assoc
               :copied (mapv :id to-copy)
               :copy-location (:location kursor))))))

(defn- paste-entities!
  []
  (let [{entities :entities kursor :cursor copied :copied loc-offset :copy-location} @state]
    (when (seq copied)
      (swap! state dissoc :copied)
      (doseq [id copied]
        (let [{:keys [content location size display]} (get entities id)
              new-id                                  (inc @entity-counter) ;; get what next id will be but don't modify counter
              new-loc                                 (mapv + (mapv - location loc-offset) (:location kursor))]
          (insert-entity! {:id new-id :content content :location new-loc :size size :display display})
          (handle-entity {:id new-id :code content})
          (server/broadcast!
           server-map
           [:div#insert-target
            {:hx-swap-oob "afterend"}
            (bc/editor (get-in @state [:entities new-id]) @state)]))))))

(defn read-edn-file [file-path]
  (with-open [rdr (io/reader file-path)]
    (edn/read-string {:readers {'badspreadsheet.cells2.CellID (fn [{:keys [id]}]
                                                                {:id id})}
                      :default (fn [_tag value] value)} (slurp rdr))))

(defn- temp-entity
  [id]
  {:id id
   :content ""
   :location [0 0]
   :size [1 1]})

(defn load-entities!
  [fname]
  (clear-state!)
  (let [entities    (read-edn-file fname)
        ids         (set (map :id entities))
        max-id      (apply max ids)
        missing-ids (set/difference (set (range max-id)) ids)
        temp-entities (map temp-entity missing-ids)
        all-entities (sort-by :id (concat entities temp-entities))]
    (doseq [entity all-entities]
      (insert-entity! entity))
    (doseq [{:keys [id content]} entities]
      (handle-entity {:id id :code content}))
    (doseq [id missing-ids]
      (remove-entity! id))
    #_(doseq [cell-id (-> @c/global-cells vals)]
      (try
        (c/touch! cell-id)
        (catch Exception _e nil)))))

(defn load!
  [fname]
  (clear-state!)
  (let [{:keys [entities] :as loaded-state} (read-edn-file fname)
        ids                                 (set (keys entities))
        _ (println ids)
        max-id                              (apply max ids)
        missing-ids                         (set/difference (set (range max-id)) ids)
        temp-entities                       (map temp-entity missing-ids)
        all-entities                        (sort-by :id (concat (vals entities) temp-entities))]
    ;; insert blank entities from id 0 to id max
    (doseq [entity all-entities]
      (insert-entity! entity))
    ;; populate the relevant ids with their actual contents from the loaded state
    (doseq [{:keys [id content]} (vals entities)]
      (handle-entity {:id id :code content}))
    ;; remove the temporary ids that didn't exist in the loaded state
    (doseq [id missing-ids]
      (remove-entity! id))
    ;; swap the rest of the state data in, excluding entities
    ;; since we've already loaded them in
    (swap! state merge (dissoc loaded-state :entities))))

(defmethod server/data-handler :make-active
  [{:keys [id]}]
  (let [id         (read-string (str/replace id #"movable" ""))
        entity     (get-in @state [:entities id])
        cursor-map {:location (:location entity)
                    :size     (:size entity)}]
    (swap! state assoc
           :active id
           :cursor cursor-map
           #_(select-keys entity [:location :size]))
    (server/broadcast!
     server-map
     (bc/cursor cursor-map @state))))

(defn- move-active-entity!
  [direction]
  (let [{:keys [active entities]} @state
        {:keys [location]}        (get entities active)]
    (when location
      (let [new-loc (mapv + location ({:left [-1 0] :right [1 0] :up [0 -1] :down [0 1]} direction))
            move!   (move-entity! new-loc active)]
        (when move!
          (let [entity (get-in @state [:entities active])]
            (server/broadcast!
             server-map
             [:<> (bc/editor entity @state) (bc/cursor entity @state)])))))))

(defn- resize-active-entity!
  [direction]
  (let [{:keys [active entities]} @state
        {:keys [size]}            (get entities active)]
    (when size
      (let [new-size (mapv + size ({:h- [-1 0] :h+ [1 0] :v- [0 -1] :v+ [0 1]} direction))
            resize!  (resize-entity! new-size active)]
        (when resize!
          (let [entity (get-in @state [:entities active])]
            (server/broadcast!
             server-map
             [:<> (bc/editor entity @state) (bc/cursor entity @state)])))))))

(defn- location-in-entity?
  [loc entity]
  (let [covers (entity-covers entity)]
    (covers loc)))

(defn- entity-at-location
  [loc]
  (let [{:keys [entities]} @state]
    (first (filter (partial location-in-entity? loc) (vals entities)))))

(defn move-camera!
  [direction]
  (let [{kursor   :cursor
         camera   :camera
         entities :entities} @state
        camera-loc           (:location camera)
        new-loc              (mapv + camera-loc ({:left [-1 0] :right [1 0] :up [0 -1] :down [0 1]} direction))]
    (swap! state assoc-in [:camera :location] new-loc)
    (server/broadcast!
     server-map
     (into [:<> (bc/cursor kursor @state)] (map #(bc/editor % @state) (vals entities))))))

(defn set-camera!
  [new-loc]
  (let [{kursor   :cursor
         entities :entities :as s} @state]
    (swap! state assoc-in [:camera :location] new-loc)
    (server/broadcast!
     server-map
     (into [:<> (bc/cursor kursor s)] (map #(bc/editor % s) (vals entities))))))

(defn- camera-move
  "Determines if the camera should move based on the cursor's new position.
   Returns the direction of movement if required."
  [[cursor-x cursor-y] [size-x size-y]]
  (let [{:keys [camera extents]} @state
        [camera-x camera-y]      (:location camera)
        [extent-x extent-y]      extents
        direction                (cond
                                   (< cursor-x camera-x)                                    :left
                                   (> cursor-x #_(+ cursor-x size-x) (+ extent-x camera-x)) :right
                                   (< cursor-y camera-y)                                    :up
                                   (> cursor-y #_(+ cursor-y size-y) (+ extent-y camera-y)) :down)]
    direction))

(defn occupied
  [loc]
  (contains? @location-index loc))

(defn- move-cursor!
  [direction-or-location]
  (let [{kursor   :cursor}      @state
        {:keys [location size]} kursor
        was-over-entity?        (occupied location)]
    (when location
      (let [[sx sy]    size
            new-loc    (if (keyword? direction-or-location)
                         (mapv + location ({:left  [-1 0]
                                            :right [(if was-over-entity? sx 1) 0]
                                            :up    [0 -1]
                                            :down  [0 (if was-over-entity? sy 1)]} direction-or-location))
                         direction-or-location)
            new-entity (when (occupied new-loc)
                         (entity-at-location new-loc))
            new-cursor (if new-entity
                         (select-keys new-entity [:location :size])
                         {:location new-loc
                          :size     size #_ (if was-over-entity? [1 1] size)})]
        (dosync
         (swap! state assoc
                :cursor new-cursor
                :active (:id new-entity))
         (when-not was-over-entity?
           (server/broadcast!
            server-map
            [:div#insert-target [:script "unfocusActiveElement();"]]))
         (when-let [direction (camera-move new-loc size)]
           (move-camera! direction))
         (server/broadcast!
          server-map
          (bc/cursor (:cursor @state) @state)))))))

(defn- move-entities-in-cursor!
  [direction-or-location]
  (let [{entities :entities kursor :cursor} @state
        entities                                               (vals entities)
        in-cursor                                              (entity-covers kursor)]
    (when (some in-cursor (set (keys @location-index)))
      (let [to-move (filter (fn [{:keys [location size]}]
                              (and (in-cursor location)
                                   (in-cursor (mapv + location (map dec size))))) entities)
            movefn! (fn [{:keys [location id]}]
                      (let [new-loc (if (keyword? direction-or-location)
                                      (mapv + location ({:left  [-1  0]
                                                         :right [ 1  0]
                                                         :up    [ 0 -1]
                                                         :down  [ 0  1]} direction-or-location))
                                      direction-or-location)]
                        (move-entity-relative! location new-loc id)))
            moved   (mapv movefn! to-move)]
        (move-cursor! direction-or-location)
        (server/broadcast!
         server-map
         (into [:<>] (map #(bc/editor % @state) moved)))))))

(defn- delete-entities-in-cursor!
  []
  (let [{entities :entities kursor :cursor} @state
        entities                                               (vals entities)
        in-cursor                                              (entity-covers kursor)]
    (when (some in-cursor (set (keys @location-index)))
      (let [to-delete (filter (fn [{:keys [location size]}]
                                  (and (in-cursor location)
                                       (in-cursor (mapv + location (map dec size))))) entities)
            deletefn! (fn [{:keys [id]}]
                        (swap! state update :entities (fn [m] (dissoc m id))))]
        (mapv deletefn! to-delete)
        (server/broadcast!
         server-map
         (into [:<>]
               (map (fn [{:keys [id]}]
                      [:div {:id          (format "entity%s" id)
                             :hx-swap-oob "outerHTML"}])
                    to-delete)))))))

(defn- resize-cursor!
  [direction-or-size]
  (let [direction        (keyword? direction-or-size)
        {kursor :cursor} @state
        {:keys [size]}   kursor]
    (when size
      (let [new-size (if direction
                       (mapv + size ({:h- [-1 0] :h+ [1 0] :v- [0 -1] :v+ [0 1]} direction-or-size))
                       direction-or-size)]
        (when (every? #(> % 0) new-size)
          (swap! state assoc-in [:cursor :size] new-size)
          (server/broadcast!
           server-map
           (bc/cursor (:cursor @state) @state)))))))

(defn- toggle-active-display!
  [direction]
  (let [{:keys [cursor active]} @state]
    (when active
      (swap! state update-in [:entities active] #(cycle-entity-display % direction))
      (server/broadcast!
       server-map
       [:<>
        (bc/cursor cursor @state)
        (bc/editor (get-in @state [:entities active]) @state)]))))

(defn- cells-by-location
  []
  (->> @state :entities vals (group-by :location)))

(defn- toggle-displays-in-area!
  [direction]
  (let [{:keys [cursor active]} @state
        locs (entity-covers cursor)
        cells (cells-by-location)
        in-cursor (map :id (mapcat #(get cells %) locs))]
    (if active
      (toggle-active-display! direction)
      (when (seq in-cursor)
        (doseq [active in-cursor]
          (swap! state update-in [:entities active] #(cycle-entity-display % direction))
          (server/broadcast!
           server-map
           (bc/editor (get-in @state [:entities active]) @state)))))))

(defn- create-entity!
  []
  (let [{:keys [cursor active]} @state]
    (when-not active
      (println "creating entity...")
      (let [entity-id (make-entity! (:location cursor) (:size cursor))
            new-entity (get-in @state [:entities entity-id])]
        (track-location! new-entity)
        (server/broadcast!
         server-map
         [:div#insert-target
          {:hx-swap-oob "afterend"}
          (bc/editor new-entity @state)])))))

(defn- delete-entity!
  []
  (println "deleting entity...")
  (let [{:keys [active]} @state]
    (when active
      (remove-entity! active)
      (server/broadcast!
       server-map
       [:div {:id          (format "entity%s" active)
              :hx-swap-oob "outerHTML"}]))))

(defn- self-removing-script
  [script-str]
  [:script#removeThis (format "%s\ndocument.getElementById('removeThis').remove();" script-str)])

(defn unfocus-active-entity!
  []
  (server/broadcast!
   server-map
   [:div#insert-target (self-removing-script "unfocusActiveElement();")]))

(defn focus-active-entity!
  []
  (let [{:keys [active]} @state]
    (when active
      (server/broadcast!
       server-map
       [:div#insert-target
        {:hx-swap-oob "afterend"}
        (self-removing-script (format "setElementFocus('%s');" active))]))))

(defmethod server/data-handler :toggle-display
  [{:keys [direction]}]
  (toggle-displays-in-area! (keyword direction)))

(defmethod server/data-handler :toggle-overlay
  [_]
  (swap! state update :overlay-on not)
  (server/broadcast! server-map (bc/cursor (:cursor @state) @state)))

(defmethod server/data-handler :keypress
  [{keys-pressed :keys}]
  (case (vec (rest keys-pressed))
    ["enter"] (do (create-entity!) (focus-active-entity!))

    ["shift" "up"]   (move-camera! :up) #_(toggle-displays-in-area! :up)
    ["shift" "down"] (move-camera! :down) #_(toggle-displays-in-area! :down)
    ["shift" "left"]  (move-camera! :left)
    ["shift" "right"] (move-camera! :right)

    ["ctrl" "n"] (create-entity!)
    ["ctrl" "d"] (delete-entity!)
    ["ctrl" "c"] (copy-entities!)
    ["ctrl" "v"] (paste-entities!)
    ["ctrl" "s"] (save! "out2.edn")

    ["left"]  (move-cursor! :left)
    ["right"] (move-cursor! :right)
    ["up"]    (move-cursor! :up)
    ["down"]  (move-cursor! :down)

    ["ctrl" "left"]  (move-entities-in-cursor! :left)
    ["ctrl" "right"] (move-entities-in-cursor! :right)
    ["ctrl" "up"]    (move-entities-in-cursor! :up)
    ["ctrl" "down"]  (move-entities-in-cursor! :down)

    #_#_ ["ctrl" "left"]  (move-active-entity! :left)
    #_#_ ["ctrl" "right"] (move-active-entity! :right)
    #_#_ ["ctrl" "up"]    (move-active-entity! :up)
    #_#_ ["ctrl" "down"]  (move-active-entity! :down)

    ["ctrl" "shift" "left"]  (if (:active @state) (resize-active-entity! :h-) (resize-cursor! :h-))
    ["ctrl" "shift" "right"] (if (:active @state) (resize-active-entity! :h+) (resize-cursor! :h+))
    ["ctrl" "shift" "up"]    (if (:active @state) (resize-active-entity! :v-) (resize-cursor! :v-))
    ["ctrl" "shift" "down"]  (if (:active @state) (resize-active-entity! :v+) (resize-cursor! :v+))
    ;; default
    (println "UNHANDLED KEYS: " keys-pressed)))

(defmethod server/data-handler :gamepad
  [{:keys [buttons]}]
  (case (vec (rest buttons))
    ["d-left"]  (move-cursor! :left)
    ["d-right"] (move-cursor! :right)
    ["d-up"]    (move-cursor! :up)
    ["d-down"]  (move-cursor! :down)

    ["L1" "d-left"]  (move-active-entity! :left)
    ["L1" "d-right"] (move-active-entity! :right)
    ["L1" "d-up"]    (move-active-entity! :up)
    ["L1" "d-down"]  (move-active-entity! :down)

    ["face-down"]  (focus-active-entity!)
    ["face-right"] (unfocus-active-entity!)

    ;; default
    (println "UNHANDLED BUTTONS: " buttons)))

(defmethod server/data-handler :mouse-event
  [{:keys [location size dragging]}]
  (let [[_ sx sy] size
        [_ x y] location]
    (when (every? pos? [sx sy])
      (resize-cursor! [sx sy]))
    (if dragging
      (move-entities-in-cursor! [x y])
      (move-cursor! [x y]))))

(defmethod server/data-handler :store-extents
  [{:keys [extents]}]
  (let [[_ w h] extents]
    (swap! state assoc :extents [w h])))

(defmethod server/data-handler :scroll
  [{:keys [direction]}]
  (let [{:keys [horizontal vertical]} direction]
    (move-camera! (keyword (or horizontal vertical)))))

(defmethod server/data-handler :set-camera
  [{:keys [position]}]
  (let [[_ x y] position]
    (set-camera! [x y])))

(defmethod server/data-handler :move-cursor
  [{:keys [position]}]
  (let [[_ x y] position]
    (move-cursor! [x y])))

(defn toggle-waypoint!
  [pos label]
  (if (contains? (:waypoints @state) pos)
    (swap! state update :waypoints (fn [m] (dissoc m pos)))
    (swap! state assoc-in [:waypoints pos]
           {:position pos
            :label    label
            :colour   (bc/random-waypoint-color)}))
  (server/broadcast! server-map (bc/cursor (:cursor @state) @state)))

(defmethod server/data-handler :toggle-waypoint
  [{:keys [position label]}]
  (let [[_ x y] position]
    (toggle-waypoint! [x y] label)))

(defmethod server/data-handler :delete
  [_]
  (delete-entities-in-cursor!))

(defn start! []
  (server/serve! (deref #'server-map)))

(defn init-grid-for-get!
  []
  (let [state @state]
    [:<>
     (into [:<>] (for [[_ entity] (:entities state)]
                   (bc/editor :init entity state)))
     (bc/cursor (:cursor state) state)
     [:script (bc/wrap-js-in-content-loaded "initKeyPressListener();")]
     [:script (bc/wrap-js-in-content-loaded (format "initMouseEventsListener(%s);" (:size state)))]
     [:script (bc/wrap-js-in-content-loaded "initGamepadListener();")]]))
