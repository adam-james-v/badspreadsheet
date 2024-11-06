(ns badspreadsheet.main
  (:require
   [clj-http.client :as http]
   [badspreadsheet.cell :as c]
   [badspreadsheet.util :as u]
   [badspreadsheet.machines :as machines]
   [badspreadsheet.sheet :as bs]
   [badspreadsheet.server :as server]
   [cheshire.core :as json]
   [clojure.string :as str])
  (:gen-class))

#_#_
(defn put!
  "Put the value `v` to the target cell `target`."
  [target v]
  (a/go (>! c/event-bus {:id target
                         :value v})))

(defn send>
  "Construct a tap function that puts the tapped value to the `target` cell."
  [target]
  (partial put! target))

(defn load-requires
  [require-form]
  (eval
   `(binding [*ns* (find-ns 'user)] ~require-form))
  "loaded.")

#_
(defn send-value>
  [target val]
  (bs/broadcast-content-into! target val))

(defn write-value>
  [target-cell-id val]
  (server/data-handler {:dispatch :code
                        :id       target-cell-id
                        :value    (str val)}))


(defn -main
  []
  #_(bs/stop-watcher)
  #_(bs/clear-state!)
  #_(bs/start-watcher2)
  (bs/start!)
  #_(bs/load! "out2.edn"))

(comment

  ;; First, load up the 'guiding-ideas.edn':
  (badspreadsheet.spreadsheet/load! "guiding-ideas.edn")
  ;; Then, show the basics
  (badspreadsheet.spreadsheet/load! "basics.edn")
  ;; The curve designer example shows some more useful controls
  (badspreadsheet.spreadsheet/load! "curve-designer.edn")
  ;; and 'level up' to surface designer
  (badspreadsheet.spreadsheet/load! "surface-designer.edn")

  (badspreadsheet.main/load-requires
   (require
    '[clojure.string :as str]
    '[svg-clj.composites :as svg.c]
    '[svg-clj.elements :as el]
    '[svg-clj.path :as path]
    '[svg-clj.transforms :as tf]
    '[svg-clj.layout :as lo]
    '[svg-clj.parametric :as p]
    '[svg-clj.utils :as u]
    '[adam.scratch :as asc]
    '[adam.arch :as arch]
    '[adam.stained-glass :as glass]
    '[forge.model :as mdl]
    '[forge.brep.curves :as b.curves]
    '[forge.brep.surfaces :as b.surfaces]
    '[forge.brep.mesh :as b.mesh]
    '[forge.frep :as ff]
    '[forge.compile.scad :as f.scad]
    '[forge.compile.sdf :as f.sdf])
   #_(import
      '[com.drew.imaging ImageMetadataReader]
      '[com.drew.metadata Metadata]
      '[java.io File]
      '[java.time LocalDateTime]
      '[java.time.format DateTimeFormatter]))

  (require '[clojure.java.io :as io])

  (defn list-files-in-directory [dir-path]
    (let [dir (io/file dir-path)]
      (if (.exists dir)
        (map #(.getAbsolutePath %) (file-seq dir))
        (println "Directory does not exist"))))



  )

(defn restore!
  [{:keys [position size content id display display-hint]}]
  (when-not (contains? (:machines @c/cells) id)
    (-> (c/formula position size content)
        (c/c-merge {:display display
                    :display-hint display-hint}))))

(defn add-form!
  [pos size form]
  (-> (c/formula pos size (str form))
      (c/c-assoc :display :value)
      (bs/render-cell)))

(defn cells-at-cursor
  []
  (let [{:keys [primary secondary]} (:cursor @bs/state)]
    {:positions [primary secondary]
     :ids (machines/get-ids-at-positions @c/cells (u/window primary secondary))}))

(defn save!
  [fname]
  (let [app-state   @bs/state
        sheet-state (-> @c/cells
                        (dissoc :grid :positions)
                        (update :machines (fn [machines-map]
                                            (update-vals machines-map (fn [m] (into {} (dissoc m :operation :memoized-operation)))))))]
    (spit fname (with-out-str
                  (clojure.pprint/pprint {:app-state   app-state
                                          :sheet-state sheet-state})))))


(def slides
  [

 {:title "Exploratory Design"
  :subtitle "Tools for Makers"
  :bullets ["Iterating towards successful design"
            "using tanglible Clojure code"]}

 {:title "me, briefly"
  :bullets ["backend dev at Metabase"
            "previously studied Mechanical Engineering"
            "love designing and making things"
            "hacking my way through it"]
  :image "resources/images/me.png"}

 #_{:title "badspreadsheet"
  :subtitle "crash course"
  :bullets ["2D canvas where you can place cells"]
  :image "resources/images/2dcanvas.jpg"}

 #_{:title "badspreadsheet"
  :subtitle "crash course"
  :bullets ["2D canvas where you can place cells"
            "Cells have 2 states: edit, value"]
  :image "resources/images/2states.jpg"}

 #_{:title "badspreadsheet"
  :subtitle "crash course"
  :bullets ["2D canvas where you can place cells"
            "Cells have 2 states: edit, value"
            "Cells can be moved and resized"]
  :image "resources/images/2states.jpg"}

 #_{:title "badspreadsheet"
  :subtitle "crash course"
  :bullets ["2D canvas where you can place cells"
            "Cells have 2 states: edit, value"
            "Cells can be moved and resized"
            "Cells can contain Clojure code/data"]
  :image "resources/images/2states.jpg"}

 #_{:title "badspreadsheet"
  :subtitle "crash course"
  :bullets ["2D canvas where you can place cells"
            "Cells have 2 states: edit, value"
            "Cells can be moved and resized"
            "Cells can contain Clojure code/data"
            "Contents of a cell are evaluated as (do ...)"]
  :image "resources/images/2states.jpg"}

 {:title "badspreadsheet"
  :subtitle "crash course"
  :bullets ["2D canvas where you can place cells"
            "Cells have 2 states: edit, value"
            "Cells can be moved and resized"
            "Cells can contain Clojure code/data"
            "Contents of a cell are evaluated as (do ...)"
            "Hiccup is rendered"]
  :image "resources/images/2states.jpg"}

 {:title "the value of references"
  :subtitle "using other cell values"
  :bullets ["a special c# form enables cell refs"
            "(c# :id 10) references cell 12"
            "(c# :position [99 7]) refs whatever cell is in that position"]
  :image "resources/images/refs.jpg"}

 {:title "the power of control"
  :subtitle "manipulating values"
  :bullets ["prefix a 'control map'"
            ":number and :slider"
            ":points-editor"
            "more to come :)"]
  :image "resources/images/controls.jpg"}

   {:title "the value of tools"
    :subtitle "Why build this?"
    :bullets ["direct manipulation is powerful + fast"
              "design needs space"
              "it's fun and cool"]}

 ])

(defn load!
  [fname]
  (let [{:keys [app-state sheet-state]} (clojure.edn/read-string (slurp fname))
        hydrated-state (-> sheet-state
                           (update :machines update-vals c/hydrate-cell!)
                           (assoc :grid {})
                           machines/process)
        highest-cell-id (apply max (keys (:machines hydrated-state)))]
    (reset! c/cell-counter highest-cell-id)
    (reset! bs/state app-state)
    (reset! c/cells hydrated-state)))
