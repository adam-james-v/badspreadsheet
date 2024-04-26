(ns badspreadsheet.main
  (:require
   [badspreadsheet.cells3 :as c]
   [badspreadsheet.spreadsheet :as bs]
   [badspreadsheet.server :as server]
   [clojure.core.async :as a :refer [>!]])
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
  (bs/load! "guiding-ideas.edn")
  ;; Then, show the basics
  (bs/load! "basics.edn")
  ;;


  )
