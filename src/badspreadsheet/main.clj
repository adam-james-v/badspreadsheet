(ns badspreadsheet.main
  (:require
   [badspreadsheet.cells2 :as c]
   [badspreadsheet.spreadsheet :as bs]
   [clojure.core.async :as a :refer [>!]])
  (:gen-class))

(defn put!
  "Put the value `v` to the target cell `target`."
  [target v]
  (a/go (>! c/event-bus {:id target
                         :value v})))

(defn send>
  "Construct a tap function that puts the tapped value to the `target` cell."
  [target]
  (partial put! target))

(defn -main
  []
  #_(bs/stop-watcher)
  (bs/clear-state!)
  (bs/start-watcher)
  (bs/start!)
  #_(bs/load-entities! "out.edn"))
