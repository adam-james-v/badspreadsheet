(ns badspreadsheet.main
  (:require
   [clj-http.client :as http]
   [badspreadsheet.cells3 :as c]
   [badspreadsheet.spreadsheet :as bs]
   [badspreadsheet.server :as server]
   [cheshire.core :as json]
   [clojure.string :as str]
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
  (badspreadsheet.spreadsheet/load! "guiding-ideas.edn")
  ;; Then, show the basics
  (badspreadsheet.spreadsheet/load! "basics.edn")
  ;; The curve designer example shows some more useful controls
  (badspreadsheet.spreadsheet/load! "curve-designer.edn")
  ;; and 'level up' to surface designer
  (badspreadsheet.spreadsheet/load! "surface-designer.edn")
  ;; and 'level up' once more to solid designer
  ()



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



  (def leaflet-payload
    [:div
     #_[:link {:rel  "stylesheet"
               :href "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"}]
     #_[:script #_(slurp "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js") {:src "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"}]
     [:div#map {:style {:height 700}}]
     [:script "var map = L.map('map').setView([51,-115], 13);"]
     [:div#script-target]])

  (def leaflet-init
    [:script "L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
    maxZoom: 19,
    attribution: '&copy; <a href=\"http://www.openstreetmap.org/copyright\">OpenStreetMap</a>'
}).addTo(map);"])

  (send-value>
   "asdf"
   leaflet-payload)

  (send-value>
   "script-target"
   leaflet-init)


  (send-value>
   "script-target"
   [:script "map.setView([51.2,-115.55], 9, { anmiation: true });"])

  (defn- add-marker
    [[x y]]
    (send-value>
     "script-target"
     [:script (format "L.marker([%s, %s]).addTo(map);" x y)]))


  (defn extract-metadata [file-path]
    (let [file (File. file-path)
          metadata (ImageMetadataReader/readMetadata file)
          dirs (.getDirectories metadata)]
      (into
       {}
       (mapcat
        (fn [directory]
          (for [tag (.getTags directory)]
            [(.getTagName tag) (.getDescription tag)]))) dirs)))




  (defn gps-data
    [m]
    (into {} (filter (fn [[k _]] (str/includes? k "GPS"))) m))

  (defn date-data
    [m]
    (into {} (filter (fn [[k _]] (str/includes? k "Date"))) m))

  (defn parse-lat-long [lat-long]
    (let [pattern #"(-?\d+)°\s*(\d+)'\s*([\d\.]+)\""
          matcher (re-matcher pattern lat-long)
          [_ d m s] (re-find matcher)]
      [(Integer/parseInt d)
       (Integer/parseInt m)
       (Double/parseDouble s)]))

  (defn parse-datetime [datetime-str]
    (let [formatter (DateTimeFormatter/ofPattern "yyyy:MM:dd HH:mm:ss")]
      (LocalDateTime/parse datetime-str formatter)))

  (defn dms->decimal
    [[d m s]]
    (if (>= d 0)
      (+ d (/ m 60) (/ s 3600))
      (- d (/ m 60) (/ s 3600))))

  (def photos-library-path "/Users/adam/Pictures/Photos Library.photoslibrary/originals")
  (def photos-library-path2 "/Users/adam/Pictures/Photos Library.photoslibrary/resources/derivatives/masters")

  (defn photos-list
    [lib-path]
    (->> (list-files-in-directory lib-path)
         (filter (fn [s]
                   (or (str/ends-with? s ".heic")
                       (str/ends-with? s ".jpeg")
                       (str/ends-with? s ".jpg"))))))

  (def original-photos (photos-list photos-library-path))
  (def compressed-photos (photos-list photos-library-path2))

  (defn- filename
    [path]
    (-> path
        (str/split #"/")
        last
        (str/split #"\.")
        first))

  (defn- find-matching-photo
    [original-path]
    (let [fname (filename original-path)]
      (first (filter #(str/includes? % fname) compressed-photos))))

  (defn filepath->data
    [path]
    (let [md   (extract-metadata path)
          keys ["GPS Latitude"
                "GPS Longitude"
                "Date/Time Original"]]
      (when (and
             (contains? md "GPS Latitude")
             (contains? md "GPS Longitude")
             (contains? md "Date/Time Original"))
        (-> (select-keys md keys)
            (update "GPS Latitude" (comp dms->decimal parse-lat-long))
            (update "GPS Longitude" (comp dms->decimal parse-lat-long))
            (update "Date/Time Original" parse-datetime)
            (assoc :original-path path)
            (assoc :compressed-path (find-matching-photo path))))))

  (def photo-maps
    (sort-by #(get % "Date/Time Original") (keep filepath->data original-photos)))

  (defn- add-markers
    [pts]
    (send-value>
     "script-target"
     [:script
      (str/join "\n" (mapv (fn [[x y]]
                             (format "L.marker([%s, %s]).addTo(map);" x y))
                           pts))]))

  (let [pts (mapv (fn [pm]
                    (let [x (get pm "GPS Latitude")
                          y (get pm "GPS Longitude")]
                      [x y]))
                  (drop-last 3 photo-maps))]
    (add-markers pts))


  (svg-clj.tools/cider-show
   (let [pts (mapv (fn [pm]
                     (let [y (get pm "GPS Latitude")
                           x (get pm "GPS Longitude")]
                       (mapv * [2.5 2.5] [x y])))
                   photo-maps)]
     (svg-clj.composites/svg (->
                              (svg-clj.elements/polyline pts)
                              (svg-clj.transforms/style {:fill "none"
                                                         :stroke "white"
                                                         :stroke-width 2})))))

  )

(def session-cookie "6afe6d69-abbe-4587-9b6e-fe59e54020e1")
(defn- mb-get-card-query
  [card-id]
  (-> (http/get (format "http://localhost:3000/api/card/%s" card-id)
                {:as      :json
                 :cookies {"metabase.SESSION" {:value session-cookie}}})
      :body
      :dataset_query))

(defn- mb-run-query
  [q]
  (-> (http/post "http://localhost:3000/api/dataset"
                 {:content-type :json
                  :cookies      {"metabase.SESSION" {:value session-cookie}}
                  :body         (json/encode q)})
      :body
      (json/decode keyword)
      :data))

(defn- rows->table
  ([rows]
   (into [:table]
         (map (fn [row]
                (into [:tr] (map (fn [d] [:td d]) row))))
         rows))
  ([header rows]
   (into [:table [:tr (mapv (fn [d] [:th d]) header)]]
         (map (fn [row]
                (into [:tr] (map (fn [d] [:td d]) row))))
         rows)))

(defn- render-results
  [{:keys [results_metadata rows]}]
  (let [{:keys [columns]} results_metadata]
    (rows->table
     (mapv :display_name columns)
     rows)))

(defn mb-explore
  []
  (let [q (mb-get-card-query 1401)]
    (mb-run-query q)))
