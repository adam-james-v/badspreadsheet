(ns badspreadsheet.main
  (:require
   [badspreadsheet.spreadsheet :as bs])
  (:gen-class))

(defn -main
  []
  (bs/start!)
  (bs/load-entities! "out.edn"))
