(ns badspreadsheet.server
  (:require
   [cheshire.core :as json]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [huff2.core :as huff]
   [org.httpkit.server :as srv]
   [ruuter.core :as ruuter])
  (:import
   (java.net ServerSocket BindException))
  (:gen-class))

;; initiate the page
(def page-head
  [:head
   [:meta {:charset "UTF-8"}]
   [:meta {:name    "viewport"
           :content "width=device-width, initial-scale=1"}]
   [:title "badspreadsheet"]
   [:link {:rel  "stylesheet"
           :type "text/css"
           :href "resources/css/style.css"}]
   [:script {:src "resources/js/htmx.min.js"}]
   [:script {:src "https://unpkg.com/idiomorph@0.3.0"}]
   [:script {:src "resources/js/ws.js"}]
   [:script {:src "resources/js/squint.core.umd.js"}]
   [:script "globalThis._sc = squint.core;"]
   [:script {:src         "resources/js/es-module-shims.js"
             :async       true
             :crossorigin "anonymous"}]
   [:script {:type "importmap"} [:hiccup/raw-html (slurp (io/resource "js/codemirror-import-map.json"))]]
   [:script {:type "module"} [:hiccup/raw-html (slurp (io/resource "js/load-codemirror.js"))]]
   [:script {:type "module"} [:hiccup/raw-html (slurp (io/resource "js/movable.js"))]]])

(defn page-body
  [content]
  [:body
   {:hx-ext "ws,idiomorph"
    :style {:margin 0
            :padding 0}}
   (into [:div {:ws-connect "/socket"}] content)])

(defn page
  [head content]
  (huff/page {:allow-raw true} [:<> head (page-body (or content [:div#broadcast-target "broadcast here!"]))]))

;; websocket handling
(def channels (atom #{}))

(defn connect! [channel]
  (println "Channel Opened")
  (swap! channels conj channel))

(defn disconnect! [channel status]
  (println "Channel Closed: " status)
  (swap! channels disj channel))

(defn handle-message! [_channel ws-message]
  (let [message (json/parse-string ws-message keyword)]
    (println "nothing to do with message: " message)))

(defn- channel-has-port?
  [port channel]
  (let [channel-str (.toString channel)]
   (str/includes? channel-str (str port))))

(defn broadcast!
  [{:keys [port]} data]
  (let [html-str (if (string? data)
                   data
                   (huff/html {:allow-raw true} data))
        f (fn [channel]
            (if (srv/open? channel)
              (when (channel-has-port? port channel)
                (srv/send! channel html-str))
              (do (srv/close channel)
                  (disconnect! channel :connection-lost))))]
    (doall (pmap f @channels))))

(defn ws-handler [request]
  (println "initial ws request made.")
  (srv/as-channel request
    {:on-receive handle-message!
     :on-close   disconnect!
     :on-open    connect!}))

(defmulti data-handler
  "Method for handling anything that comes in on the /data endpoint."
  (fn [{:keys [dispatch]}]
    (keyword dispatch)))

(defmethod data-handler :default
  [data]
  (println "Unhandled data: " data)
  (println "You may have to implement `badspreadsheet.server/data-handler`.")
  (println "Dispatch occurs off the value in the `:dispatch` key of the data."))

;; routing

(defn parse-request
  [{:keys [params body]}]
  {:body (json/parse-string (slurp body) keyword vector)
   :params params})

(defn- resource-response
  [{:keys [params]}]
  (let [{:keys [file partial-path]} params
        ext  (last (str/split file #"\."))]
    {:status  200
     :headers {"Content-Type" (format "text/%s" (if (= ext "js") "javascript" ext))}
     :body    (slurp (io/resource (str partial-path "/" file)))}))

(def default-routes
  {["/resources/:partial-path/:file" :get] resource-response
   ["/socket" :get]                        ws-handler
   ["/" :get]                              (fn [_] {:body (page page-head [[:div#broadcast-target "Broadcast Stuff Here!"]])})
   ["/data" :post]                         (fn [req]
                                             (let [{:keys [body]} (parse-request req)]
                                               (data-handler body)
                                               {:body ""}))})

(defn- routes-map->routes
  [m]
  (mapv (fn [[[path method] response-fn]]
          {:path path
           :method method
           :response response-fn}) m))

(defonce servers (atom {}))

(defn stop-server [{:keys [port]}]
  (let [server (get @servers port)]
    (when server
      (server :timeout 100)
      (swap! servers dissoc port))))

;; https://github.com/prestancedesign/get-port/blob/main/src/prestancedesign/get_port.clj
(defn- get-available-port
  "Return a random available TCP port in allowed range (between 1024 and 65535) or a specified one"
  ([] (get-available-port 0))
  ([port]
   (with-open [socket (ServerSocket. port)]
     (.getLocalPort socket))))

(defn get-port
  "Get an available TCP port according to the supplied options.
  - A preferred port: (get-port {:port 3000})
  - A vector of preferred ports: (get-port {:port [3000 3004 3010]})
  - Use the `make-range` helper in case you need a port in a certain (inclusive) range: (get-port {:port (make-range 3000 3005)})
  No args return a random available port"
  ([] (get-available-port))
  ([opts]
   (loop [port (:port opts)]
     (let [result
           (try
             (get-available-port (if (number? port) port (first port)))
             (catch Exception e (instance? BindException (.getCause e))))]
       (or result (recur (if (number? port) 0 (next port))))))))

(defn serve!
  [{:keys [port routes-map] :as server-map}]
  (let [routes         (routes-map->routes (merge default-routes routes-map))
        app            (fn [request] (ruuter/route routes request))
        available-port (get-port {:port (range 8000 9000)})]
    (if-not port
      (println "Must provide a port. If you need one, this port is available: " available-port)
      (do (stop-server server-map)
          (swap! servers assoc port (srv/run-server app {:port port}))
          (println "Server started on Port: " port)))))
