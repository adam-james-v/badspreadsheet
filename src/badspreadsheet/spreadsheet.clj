(ns badspreadsheet.spreadsheet
  (:require
   [badspreadsheet.cells :as c]
   [badspreadsheet.server :as server]
   [clojure.data :as data]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [clojure.walk :as walk]
   [huff2.core :as h]
   [nextjournal.markdown :as md]
   [nextjournal.markdown.transform :as md.transform]
   [overtone.at-at :as at]
   [squint.compiler]
   [svg-clj.elements :as el]
   [svg-clj.path :as path]
   [svg-clj.transforms :as tf]))

(def state-map
  {:size     20
   :active   nil
   :camera   {:location [0 0]}
   :cursor   {:location [0 0] :size [1 1]}
   :occupied #{}
   :entities {}
   :timers   {}})

(defonce state
  (atom state-map))

(defonce ^:private entity-counter (atom -1))
;; threadpool and timing stuff
(defonce runner-pool (at/mk-pool))

(defn clear-state! []
  (reset! c/global-cells {})
  (reset! c/cell-counter -1)
  (reset! entity-counter -1)
  (at/stop-and-reset-pool! runner-pool)
  (reset! state state-map))

;; kinda nice palette
;; https://colorhunt.co/palette/4d455de96479f5e9cf7db9b6
;; 4D455D Black
;; E96479 red
;; F5E9CF beige
;; 7DB9B6 teal

;; https://colorhunt.co/palette/ece3ce7390724f6f523a4d39
;; ECE3CE ;; lighter beige
;; 739072 ;; green light
;; 4F6F52 ;; green medium
;; 3A4D39 ;; green dark

(def black-col "#4D455D")
(def red-col "#E96479")
(def beige-col "#F5E9CF")
(def teal-col "#7DB9B6")

(def beige-l-col "#ECE3CE")
(def green-l-col "#739072")
(def green-m-col "#4F6F52")
(def green-d-col "#3A4D39")

(def lavender-col "#D1CFE2")
(def violet-col "#736372")

(def pastel-green-col "#D6F6DD")
(def pastel-purple-col "#DAC4F7")
(def pastel-red-col "#F4989C")
(def pastel-brown-col "#EBD2B4")
(def pastel-blue-col "#ACECF7")

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
                                           [[:div#bg.bg-grid
                                             {:style {:position "fixed"
                                                      :width    "110vw"
                                                      :height   "110vh"}}
                                             [:div#entity-container
                                              {:style {:width  "110vw"
                                                       :height "110vh"}}
                                              (init-grid-for-get!)]]])})}})

(defn style
  "Merge a style map into the given element."
  [[k props & content] style-map]
  (into [k (merge props style-map)] content))

(defn table
  [items w]
  (let [rows (partition-all w items)]
    (into [:table]
          (for [row rows]
            (into [:tr]
                  (for [item row]
                    [:td {:style {:padding 0 :margin 0}} item]))))))

(defn- new-entity-id [] (swap! entity-counter inc))

(defn- render-markdown-string
  [s]
  (-> s md/parse md.transform/->hiccup (assoc 0 :div.markdown)))

(defn- hiccup?
  [value]
  (when (seqable? value)
    (and (vector? value)
         (keyword? (first value)))))

(defn render-value
  [value]
  (cond
    (and (vector? value)
         (not (keyword? (first value)))) (pr-str value)
    (hiccup? value)                      value
    :else                                (str value)))

(comment
  ;; what are entities?
  ;; they have a few properties:


  {:location [0 2] ;; grid X and Y
   :size     [1 1] ;; grid unit width and height
   :display  :value ;; one of :value :content (add more in future, eg. :raw-value)
   :cell     :the-cell-id
   :watcher  :the-watcher-cell-id
   :content  "string of the editor's content"
   :error    "maybe put errors here?"})

(defn- watch-fn
  [id]
  (fn [value]
    (server/broadcast!
     server-map
     [:span {:id (format "value-%s" id)} (render-value value)])
    value))

(defn make-entity
  [loc size]
  (let [id (new-entity-id)
        cell (c/formula (fn [] nil))]
    {:id id
     :location loc
     :size size
     :display :content
     :cell cell
     :watcher (c/formula (watch-fn id) cell)
     :content ""
     :error nil
     :timers nil}))

(defn- entity-covers
  [{:keys [location size]}]
  (let [[x y] location
        [nx ny] size]
    (into #{}
          (for [sx (range nx)
                sy (range ny)]
            [(+ x sx) (+ y sy)]))))

(defn fix-occupied!
  []
  (swap! state assoc :occupied
         (apply set/union (map entity-covers (vals (:entities @state))))))

(defn make-entity!
  ([loc] (make-entity! loc [1 1]))
  ([loc size]
   (let [lstate @state
         occupied-locations (:occupied lstate)]
     #_(when-not (occupied-locations loc))
     (let [{:keys [id] :as entity} (make-entity loc size)
           covered                 (entity-covers entity)
           xf                      (fn [state]
                                     (-> state
                                         (assoc :active id
                                                :occupied (set/union occupied-locations covered))
                                         (assoc-in [:entities id] entity)
                                         (update :locations (partial merge-with set/union)
                                                 (zipmap covered (repeat #{id})))))]
       (swap! state xf)
       (doseq [loc-cell (map #(get-in lstate [:location-cells %]) covered)]
         (when loc-cell
           (c/touch! loc-cell)))
       id))))

(defn- remove-nil-vals
  [m]
  (into {} (remove #(nil? (second %)) m)))

(defn- remove-empty-sets
  [m]
  (into {} (filter #(seq (second %)) m)))

(defn remove-entity!
  [id]
  (let [{:keys [occupied] :as lstate}     @state
        {:keys [cell watcher] :as entity} (get-in lstate [:entities id])]
    (when entity
      (let [old-covering (entity-covers entity)
            xf           (fn [state]
                           (-> state
                               (assoc :occupied (set/difference occupied old-covering))
                               (update :entities dissoc id)
                               (update :locations update-vals #(set/difference % #{id}))
                               (update :locations remove-nil-vals)
                               (update :locations remove-empty-sets)))]
        (swap! state xf)
        (doseq [loc-cell (map #(get-in lstate [:location-cells %]) old-covering)]
          (when loc-cell
            (c/touch! loc-cell)))
        (c/destroy! watcher)
        (c/destroy! cell)))))

(defn move-entity!
  [new-loc entity-id]
  (let [lstate @state
        {:keys [size] :as entity} (get-in lstate [:entities entity-id])
        old-covering              (entity-covers entity)
        new-covering              (entity-covers {:location new-loc :size size})
        other-covering            (set/difference (:occupied lstate) old-covering)]
    (when entity
      (let [moved-entity (assoc entity :location new-loc)
            xf           (fn [state]
                           (-> state
                               (assoc :occupied (set/union new-covering other-covering))
                               (assoc-in [:entities entity-id] moved-entity)
                               (update :locations update-vals #(set/difference % #{entity-id}))
                               (update :locations remove-nil-vals)
                               (update :locations remove-empty-sets)
                               (update :locations (partial merge-with set/union)
                                       (zipmap new-covering (repeat #{entity-id})))))]
        (swap! state xf)
        (doseq [loc-cell (map #(get-in lstate [:location-cells %]) (set/union old-covering new-covering))]
          (when loc-cell
            (c/touch! loc-cell)))
        moved-entity))))

(defn resize-entity!
  [new-size entity-id]
  (when (every? #(> % 0) new-size)
    (let [lstate @state
          {:keys [location] :as entity} (get-in lstate [:entities entity-id])
          old-covering                  (entity-covers entity)
          new-covering                  (entity-covers {:location location :size new-size})
          other-covering                (set/difference (:occupied lstate) old-covering)]
      (when entity
        (let [resized-entity (assoc entity :size new-size)
              xf             (fn [state]
                               (-> state
                                   (assoc :occupied (set/union new-covering other-covering))
                                   (assoc-in [:entities entity-id] resized-entity)
                                   (update :locations update-vals #(set/difference % #{entity-id}))
                                   (update :locations remove-nil-vals)
                                   (update :locations remove-empty-sets)
                                   (update :locations (partial merge-with set/union)
                                           (zipmap new-covering (repeat #{entity-id})))))]
          (doseq [loc-cell (map #(get-in lstate [:location-cells %]) (set/union old-covering new-covering))]
            (when loc-cell
              (c/touch! loc-cell)))
          (swap! state xf))))))

(def display-sequence
  [:content :note :value :control :none])

(defn cycle-entity-display
  ([entity] (cycle-entity-display entity :up))
  ([entity direction]
   (let [vals         (conj (vec (rest display-sequence)) (first display-sequence))
         next-display (case direction
                        :up   (zipmap display-sequence vals)
                        :down (zipmap vals display-sequence))]
     (update entity :display next-display))))

(defn maybe-load-string [s]
  (when (and s (> (count s) 0))
    (try
      (load-string (format "(in-ns 'badspreadsheet.spreadsheet)\n%s" s))
      (catch Exception e
        (println "Error loading string: " (.getMessage e))
        (println "String is: " s)
        nil))))

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
      (println "Error reading string.")
      nil)))

(def ^:private sharp-forms
  {'c# 'badspreadsheet.spreadsheet/c# ;; value by id
   't# 'badspreadsheet.spreadsheet/t# ;; timer tick every n ms
   'l# 'badspreadsheet.spreadsheet/l# ;; value by location
    })

(defn collect-sharp-forms
  [form]
  (letfn [(collect [x]
            (cond
              (and (list? x) (seq x) ((set (keys sharp-forms)) (first x))) [x]
              (coll? x) (mapcat collect x)
              :else []))]
    (vec (distinct (collect form)))))

;; this needs to return the cell-id
(defn c# [entity-id] (get-in @state [:entities entity-id :cell]))

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

(defn- make-location-cell!
  [loc]
  (let [cell (c/formula
              (fn []
                (let [state      @state
                      ids        (get-in state [:locations loc])
                      value-cell (get-in state [:entities (first ids) :cell])]
                  (when value-cell
                    (c/value value-cell)))))]
    (swap! state assoc-in [:location-cells loc] cell)
    cell))

(defn l#
  [loc]
  (let [loc-cell-id (get-in @state [:location-cells loc])]
    (if loc-cell-id
      loc-cell-id
      (make-location-cell! loc))))

(defn formulize
  [form]
  (let [sharps           (collect-sharp-forms form)
        syms             (mapv (fn [[sym id]] (symbol (format "%s%s" sym id))) sharps)
        smap             (zipmap sharps syms)
        input-cell-forms (vec (walk/postwalk-replace sharp-forms sharps))]
    {:fun         (eval `(fn ~syms ~(walk/postwalk-replace smap form)))
     :form        `(fn ~syms ~(walk/postwalk-replace smap form))
     :input-cells (when (seq input-cell-forms) (map eval input-cell-forms))}))

(defn maybe-formulize
  [form]
  (try
    (formulize form)
    (catch Exception e
      (println "Error formulizing form."))))

(defn reset-cell!
  [cell-id form]
  (let [{:keys [fun _form input-cells]} (maybe-formulize form)]
    (when fun
      (apply c/reset-function! cell-id fun input-cells))))

(defn compile-string
  [clj-str]
  (-> (squint.compiler/compile-string* clj-str {:core-alias "_sc"}) :body))

(defn wrap-js-in-content-loaded
  [js-str]
  (format "document.addEventListener('DOMContentLoaded', function () { %s });" js-str))

(defn clj->js
  [form]
  (let [form (walk/postwalk
              (fn [item]
                (if (qualified-symbol? item)
                  (symbol (name item))
                  item))
              form)]
    (-> form pr-str compile-string)))

;; display types:
;; content -> show the codemirror editor, hide value
;; note    -> show a markdown render of the content
;; value   -> show the value render, hide editor
;; control -> show a control based on the content. Eg. number = slider or an ^v input boxes
;; none    -> completely hide the cell (not sure what indicator should be left?, maybe just a tiny triangle in top left)

(defn- number-content?
  [content]
  (number? (maybe-read-string content)))

(defn- id-info-chip
  [content display [x y] size]
  [:div.id-info {:style {:display  (if (= display :none) "auto" "none")
                         :position "absolute"
                         :opacity  0.4
                         :top      y
                         :left     x
                         :filter   "drop-shadow(0px 2px 1px rgba(9, 9, 10, 0.65))"
                         :z-index  "190"}}

   [:div {:style {:position         "relative"
                  :left             0
                  :top              0
                  :width            (* 3  size)
                  :height           (* 1.5 size)
                  :line-height      (* 1.5 size)
                  :border-radius    size
                  :text-align       "center"
                  :background-color violet-col
                  :font-size        10
                  :font-weight      "bold"
                  :font-family      "monospace"
                  :color            lavender-col
                  :z-index          "191"}}
    [:span {:style {:display        "inline-block"
                    :line-height    "normal"
                    :vertical-align "middle"}} content]]
   #_[:div {:style {:position         "relative"
                    :top              (* -0.55 size)
                    :left             (- (* 1.5 size) (* 0.375 size))
                    :width            (* 0.75 size)
                    :height           (* 0.75 size)
                    :border-radius    3
                    :background-color violet-col
                    :transform        "rotate(45deg)"
                    :z-index          "189"}}]])

(defn editor
  ([entity] (editor false entity))
  ([init? {:keys [display location content id size]}]
   (let [wrap                (if init? wrap-js-in-content-loaded identity)
         {global-size :size} @state
         [x y]               location
         [nx ny]             size
         w                   (* nx global-size)
         h                   (* ny global-size)
         left                (* x global-size)
         top                 (* y global-size)]
     [:div {:id (format "entity%s" id)}
      (id-info-chip (format "ID: %s" id) display
                    [(+ left (- (- w (* 1.75  global-size)) (* 1.5  global-size)))
                     (+ top (- (- h (* 0.25  global-size)) (* 1.625  global-size)))]
                    global-size)
      [:div {:id    (format "movable%s" id)
             :style (merge
                     {:box-sizing       "border-box"
                      :overflow         "hidden"
                      :position         "absolute"
                      :left             left
                      :top              top
                      :width            w
                      :height           h}
                     (when (#{:none :input :value} display)
                       {:background-color "rgba(255,255,255,0.2);"})
                     (when (= display :content)
                       {:overflow "visible"
                        :filter   "drop-shadow(0px 2px 2px rgba(9, 9, 10, 0.35))"}))}
       [:div {:id    id
              :style {:display (if (= display :content) "auto" "none")
                      :height  (* ny global-size)}}
        [:hiccup/raw-html content]]
       [:div {:style {:display (if (#{:none :content} display) "none" "auto")}}
        [:span {:id (format "value-%s" id)}
         (let [value (c/value (c# id))]
           (if (not (hiccup? value))
             ((if (= display :note)
                #(render-markdown-string (str %))
                str)
              value)
             value))]]]
      ;; scripts
      [:<>
       (when (and
              (= display :control)
              (number-content? content))
         [:script ((if init? wrap-js-in-content-loaded identity) (clj->js `(makeNumberInput ~id)))])
       (when (= display :content)
         [:script (wrap (clj->js `(createEditorInstance ~id)))])
       [:script (wrap (format "attachEntityListeners('movable%s');" id))]]])))

(defn- cursor-icon
  [w h]
  (let [t      5
        l      6
        st     1
        pts    [[0 0]
                [0 (- l)] [(* -0.5 t) (- (+ l (* 0.5 t)))] [(- t) (- l)]
                [(- t) 0] [0 t]
                [l t] [(+ l (* 0.5 t)) (* 0.5 t)] [l 0]]
        corner (-> (path/polygon pts)
                   (tf/style {:stroke       green-d-col
                              :stroke-width st
                              :fill         green-m-col}))]
    [:svg
     {:width  (+ w (* (+ st t) 2))
      :height (+ h (* (+ st t) 2))
      :xmlns  "http://www.w3.org/2000/svg"}
     (-> (el/g
          (-> (el/rect w h)
              (tf/translate [(* w 0.5) (* h 0.5)])
              (tf/style {:stroke green-l-col :fill "none"}))
          (-> (el/rect (+ w t (* st 0.5)) (+ h t (* st 0.5)))
              (tf/translate [(* w 0.5) (* h 0.5)])
              (tf/style {:stroke green-l-col :fill "none" :stroke-width 1 :opacity 1}))
          (-> (el/g
               #_(-> corner (tf/rotate  90) (tf/translate [0 0]))
               (-> corner (tf/rotate 180) (tf/translate [w 0]))
               #_(-> corner (tf/rotate 270) (tf/translate [w h]))
               (-> corner (tf/rotate   0) (tf/translate [0 h])))
              (tf/style {:filter "drop-shadow(0px 0.5px 0.25px rgba(9, 9, 10, 0.35))"})))
         (tf/translate [(+ t st) (+ t st)]))]))

(defn cursor
  [{:keys [location size]}]
  (let [global-size (:size @state)
        [x y]       location
        [nx ny]     size]
    [:<>
     [:div#insert-target {:hx-swap-oob "afterend"}]
     [:div#cursor.fade-in
      {:style {:pointer-events "none"}}
      (let [pos-indicator-str      (format "[%s %s]" x y)
            approx-pos-indicator-w (* 0.45 (count pos-indicator-str))]
        [:div {:style {:position "absolute"
                       :left     (* (- x approx-pos-indicator-w) global-size)
                       :top      (* (dec y) global-size)}}
         pos-indicator-str])
      (let [pos-indicator-str (format "[%s %s]" (+ x nx) (+ y ny))]
        [:div {:style {:position "absolute"
                       :left     (* (+ x nx) global-size)
                       :top      (* (+ y ny) global-size)}}
         pos-indicator-str])
      [:div {:style {:box-sizing     "border-box"
                     :border-radius  4
                     :position       "absolute"
                     :z-index        "90"
                     :pointer-events "none"
                     :margin         -6
                     :left           (* x global-size)
                     :top            (* y global-size)
                     :width          (* nx global-size)
                     :height         (* ny global-size)}}
       (cursor-icon (* nx global-size) (* ny global-size))]]]))

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
          (editor entity))))

(defn- handle-entity
  ([req] (handle-entity req false))
  ([{:keys [id code]} init?]
   (let [code         (if init? "" code) ;; intentionally initialize entities with empty code for loading purposes
         id           (if (string? id) (parse-long id) id)
         form         (maybe-read-string code)
         cell-id      (get-in @state [:entities id :cell])
         prev-content (get-in @state [:entities id :content])]
     (when (or
            init?
            (= code "")
            (and form
                 #_(not= code prev-content)))
       (swap! state assoc-in [:entities id :content] code)
       (reset-cell! cell-id form)))))

(defmethod server/data-handler :code
  [req]
  (handle-entity req))

(defn save-entities!
  [fname]
  (let [{:keys [entities]} @state
        cleaned (mapv (fn [entity] (dissoc entity :cell :watcher)) (vals entities))]
    (spit fname (vec (sort-by :id cleaned)))))

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
            (editor (get-in @state [:entities new-id]))]))))))

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
    (doseq [cell-id (-> @c/global-cells :cells vals)]
      (try
        (c/touch! cell-id)
        (catch Exception _e nil)))
    (doseq [{:keys [watcher]} (-> @state :entities vals)]
      (c/touch! watcher))))

(defmethod server/data-handler :make-active
  [{:keys [id]}]
  (let [id (read-string (str/replace id #"movable" ""))
        entity (get-in @state [:entities id])]
    (swap! state assoc
           :active id
           :cursor (select-keys entity [:location :size]))
    (server/broadcast!
     server-map
     (cursor (get-in @state [:entities id])))))

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
             [:<> (editor entity) (cursor entity)])))))))

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
             [:<> (editor entity) (cursor entity)])))))))

(defn- location-in-entity?
  [loc entity]
  (let [covers (entity-covers entity)]
    (covers loc)))

(defn- entity-at-location
  [loc]
  (let [{:keys [entities]} @state]
    (first (filter (partial location-in-entity? loc) (vals entities)))))

(defn- move-cursor!
  [direction-or-location]
  (let [{occupied :occupied kursor :cursor} @state
        {:keys [location size]}             kursor
        was-over-entity?                    (occupied location)]
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
                          :size     (if was-over-entity? [1 1] size)})]
        (dosync
         (swap! state assoc
                :cursor new-cursor
                :active (:id new-entity))
         (when-not was-over-entity?
           (server/broadcast!
            server-map
            [:div#insert-target [:script "unfocusActiveElement();"]]))
         (server/broadcast!
          server-map
          (cursor (:cursor @state))))))))

(defn- move-entities-in-cursor!
  [direction]
  (let [{entities :entities occupied :occupied kursor :cursor} @state
        entities                                               (vals entities)
        in-cursor                                              (entity-covers kursor)]
    (when (some in-cursor occupied)
      (let [to-move (filter (fn [{:keys [location size]}]
                              (and (in-cursor location)
                                   (in-cursor (mapv + location (map dec size))))) entities)
            movefn! (fn [{:keys [location id]}]
                      (let [new-loc (mapv + location ({:left [-1 0] :right [1 0] :up [0 -1] :down [0 1]} direction))]
                        (move-entity! new-loc id)))
            moved   (mapv movefn! to-move)]
        (move-cursor! direction)
        (server/broadcast!
         server-map
         (into [:<>] (map editor moved)))))))

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
           (cursor (:cursor @state))))))))

(defn- toggle-active-display!
  [direction]
  (let [{:keys [active]} @state]
    (when active
      (swap! state update-in [:entities active] #(cycle-entity-display % direction))
      (server/broadcast!
       server-map
       (editor (get-in @state [:entities active]))))))

(defn- create-entity!
  []
  (let [{:keys [cursor active]} @state]
    (when-not active
      (println "creating entity...")
      (let [entity-id (make-entity! (:location cursor) (:size cursor))]
        (server/broadcast!
         server-map
         [:div#insert-target
          {:hx-swap-oob "afterend"}
          (editor (get-in @state [:entities entity-id]))])))))

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

(defmethod server/data-handler :keypress
  [{keys-pressed :keys}]
  (fix-occupied!)
  (case (vec (rest keys-pressed))
    ["enter"] (do (create-entity!) (focus-active-entity!))

    ["shift" "up"]   (toggle-active-display! :up)
    ["shift" "down"] (toggle-active-display! :down)

    ["ctrl" "n"] (create-entity!)
    ["ctrl" "d"] (delete-entity!)
    ["ctrl" "c"] (copy-entities!)
    ["ctrl" "v"] (paste-entities!)
    ["ctrl" "s"] (save-entities! "out.edn")

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
  (fix-occupied!)
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
  [{:keys [location size]}]
  (let [[_ sx sy] size
        [_ x y] location]
    (when (every? #(> % 1) [sx sy])
      (resize-cursor! [sx sy]))
    (move-cursor! [x y])))

(defn start! []
  (server/serve! (deref #'server-map)))

(defn init-grid-for-get!
  []
  (let [state @state]
    [:<>
     (into [:<>] (for [[_ entity] (:entities state)]
                   (editor :init entity)))
     (cursor (:cursor state))
     [:script (wrap-js-in-content-loaded "initKeyPressListener();")]
     [:script (wrap-js-in-content-loaded (format "initMouseEventsListener(%s);" (:size state)))]
     [:script (wrap-js-in-content-loaded "initGamepadListener();")]]))
