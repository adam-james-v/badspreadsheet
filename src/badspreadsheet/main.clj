(ns badspreadsheet.main
  (:require
   [clj-http.client :as http]
   [badspreadsheet.cell :as c]
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
  (bs/clear-state!)
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
    '[svg-clj2.composites :as comp :refer [svg]]
    '[svg-clj2.elements :as el]
    '[svg-clj2.transforms :as tf]
    '[svg-clj2.layout :as lo]
    '[svg-clj2.parametric :as p]
    '[svg-clj2.utils :as u]
    '[adam.scratch :as asc]
    '[forge.model :as mdl]
    '[forge.brep.curves :as b.curves]
    '[forge.brep.surfaces :as b.surfaces]
    '[forge.brep.mesh :as b.mesh]
    '[forge.frep :as ff]
    '[forge.compile.scad])
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
