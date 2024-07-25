(ns badspreadsheet.components
  (:require
   [badspreadsheet.cell :as c]
   [cheshire.core :as json]
   [clojure.data.codec.base64 :as b64]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.pprint :as pp]
   [clojure.walk :as walk]
   [nextjournal.markdown :as md]
   [nextjournal.markdown.transform :as md.transform]
   [scicloj.kindly-advice.v1.api :as kindly-advice]
   [scicloj.kindly.v4.kind :as kind]
   [squint.compiler :as squint]
   [svg-clj2.elements :as el]
   [svg-clj2.layout :as lo]
   [svg-clj2.parametric :as p]
   [svg-clj2.path :as path]
   [svg-clj2.transforms :as tf]
   [svg-clj2.utils :as u]))

(defn file-to-byte-array [relative-path]
  (let [file (io/file relative-path)                       ; Create a file object
        resource-stream (io/input-stream file)]            ; Open input stream for the file
    (let [byte-array-output-stream (java.io.ByteArrayOutputStream.)]
      (io/copy resource-stream byte-array-output-stream)   ; Copy stream data to byte array output stream
      (.toByteArray byte-array-output-stream))))           ; Convert byte array output stream to byte array

(defn bytes->b64u
  "Encode data to base64 byte array (using url-safe variant)."
  [^bytes data]
  (let [encoder (-> (java.util.Base64/getUrlEncoder)
                    #_(.withoutPadding))]
    (.encode encoder data)))

(defn encode-image-to-base64
  [image-path]
  (let [bytes (file-to-byte-array image-path)]
    (-> bytes bytes->b64u (String. "UTF-8"))))

(defn compile-string
  [clj-str]
  (-> (squint.compiler/compile-string* clj-str {:core-alias "_sc"}) :body))

(defn clj->js
  [form]
  (let [form (walk/postwalk
              (fn [item]
                (if (qualified-symbol? item)
                  (symbol (name item))
                  item))
              form)]
    (-> form pr-str compile-string)))

(defn wrap-js-in-content-loaded
  [js-str]
  (format "document.addEventListener('DOMContentLoaded', function () { %s });" js-str))

(defn fe-send
  [m]
  (format "send(%s)" (json/encode m)))

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

(defn- visible?
  [[cx cy :as camera] extents [px py]]
  (let [[ex ey] (mapv + camera extents)]
    (and (<= cx px ex)
         (<= cy py ey))))

(defn home-point
  [{:keys [size camera extents]}]
  (when extents
    (let [c                 (:location camera)
          [renderx rendery] (mapv #(* -1 %) c)
          [ex ey]           extents
          px                (* size (max 0.25 (min renderx (- ex 0.25))))
          py                (* size (max 0.25 (min rendery (- ey 0.25))))]
      (when (visible? c extents [0 0])
        [:div#origin
         {:style   {:position "absolute"
                    :left px
                    :top  py}}
         [:div
          {:style {:width         12
                   :height        12
                   :left          (- 6)
                   :top           (- 6)
                   :box-sizing    "border-box"
                   :border-radius 6
                   :background    "linear-gradient(35deg, #811CFB 10%, #F4ACF0 100%)"
                   :position      "relative"}}]
         [:div
          {:onclick (fe-send {:dispatch :set-camera :position [(- (int (/ ex 2))) (- (int (/ ey 2)))]})
           :style {:cursor          "crosshair"
                   :width           15
                   :height          15
                   :left            (- 7.5)
                   :top             (- (+ 7.5 12))
                   :box-sizing      "border-box"
                   :border          "1.5px solid rgba(235, 215, 235, 0.4)"
                   :border-radius   7.5
                   :backdrop-filter "blur(1px)"
                   :position        "relative"}}]]))))

(defn random-waypoint-color
  []
  (let [[a b c d e f] (repeatedly 6 #(+ 20 (rand-int 236)))]
    (format "rgb(%s,%s,%s)" a b c)
    #_(str "linear-gradient(" (rand-int 360) "deg, rgb(" a "," b "," c ") 10%, rgb(" d "," e "," f ") 100%)")))

(defn waypoint
  [{:keys [position label colour]} {:keys [size camera extents]}]
  (let [c                 (:location camera)
        [wx wy :as w]     position
        [renderx rendery] (mapv + w (mapv #(* -1 %) c))
        [ex ey]           extents
        px                (* size (max 0.25 (min renderx (- ex 0.25))))
        py                (* size (max 0.25 (min rendery (- ey 0.25))))]
    (when (visible? c extents w)
      [:div.waypoint
       {:id    (format "waypoint-%s-%s" wx wy)
        :style {:position "absolute"
                :left     px
                :top      py}}
       [:span {:style {:height   0
                       :position "relative"
                       :display  "block"}} (or label "")]
       [:div
        {:style {:width         12
                 :height        12
                 :left          (- 6)
                 :top           (- 6)
                 :box-sizing    "border-box"
                 :border-radius 6
                 :background    colour
                 :position      "relative"}}]
       [:div
        {:onclick
         (str/join
          ";\n"
          [(fe-send {:dispatch :set-camera :position [(- wx (int (/ ex 2)))
                                                      (- wy (int (/ ey 2)))]})
           #_(fe-send {:dispatch :move-cursor :position [(inc wx) (inc wy)]})])
         :style {:cursor          "crosshair"
                 :width           15
                 :height          15
                 :left            (- 7.5)
                 :top             (- (+ 7.5 12))
                 :box-sizing      "border-box"
                 :border          "1.5px solid rgba(235, 215, 235, 0.4)"
                 :border-radius   7.5
                 :backdrop-filter "blur(1px)"
                 :position        "relative"}}]])))

(defn collect-location-refs
  [form]
  (letfn [(collect [x]
            (cond
              (and (list? x) (seq x) ((set ['l#]) (first x))) [(second x)]
              (coll? x) (mapcat collect x)
              :else []))]
    (vec (distinct (collect form)))))

(defn collect-cell-refs
  [form]
  (letfn [(collect [x]
            (cond
              (and (list? x) (seq x) ((set ['c#]) (first x))) [(second x)]
              (coll? x) (mapcat collect x)
              :else []))]
    (vec (distinct (collect form)))))

(defn v*
  [v1 v2]
  (mapv * v1 v2))

(defn- iso-triangle
  [b h]
  (let [b-half (/ b 2.0)
        h-half (/ h 2.0)]
    (el/polygon [[(- b-half) (- h-half)]
                 [ b-half (- h-half)]
                 [0 h-half]])))

(defn render-refs
  [{:keys [id form location size]} {grid-size :size camera :camera :as state}]
  (let [{camera-loc :location} camera
        [x0 y0]                (mapv - (mapv + location (v* size [0.5 0.5])) camera-loc)
        loc-refs               (map (fn [loc]
                                      (mapv - (mapv + loc [0.5 0.5]) camera-loc)) (collect-location-refs form))
        cell-locs              (map (fn [id]
                                      (let [{:keys [location size loc-ref]} (get-in state [:entities id] {:location (mapv - location [0 (second size)])
                                                                                                          :size     size
                                                                                                          :loc-ref  true})]
                                        (mapv - (mapv + location (v* size [0.5 0.5])) camera-loc)))
                                    (collect-cell-refs form))]
    (into [:g {:id (format "refs_for_%s" id)}]
          (mapcat
           (fn [[xr yr]]
             (let [[a b c d :as pts] (map #(v* % [grid-size grid-size])
                                          [[x0 y0]
                                           [(+ x0 (* (- xr x0) 0.25)) y0]
                                           [xr  (- yr (* (- yr y0) 0.25))]
                                           [xr yr]])]
               [#_[:circle {:r 5 :cx (* xr grid-size) :cy (* yr grid-size)}]
                (-> (iso-triangle 14 20)
                    (tf/translate a)
                    #_(tf/rotate (u/angle-from-pts (mapv + b [0 -10]) b a)))
                (-> (apply path/bezier pts)
                    (tf/style {:fill   "none"
                               :stroke "black"}))]))
           (concat cell-locs loc-refs)))))

(defn information-overlay
  [{:keys [entities overlay-on waypoints] :as state}]
  (when overlay-on
    (into
     [:svg#information-overlay
      {:style {:pointer-events "none"
               :width          "100vw"
               :height         "100vh"
               :position       "absolute"}}
      [:g#overlay-controls
       (-> (el/rect 30 30)
           (tf/translate [20 20])
           (tf/style {:style        {:pointer-events "auto"}
                      :label        "Toggle Information Overlay."
                      :onclick      (fe-send {:dispatch :toggle-overlay})
                      :position     "absolute"
                      :bottom       "-10px"
                      :stroke-width 1
                      :stroke       "black"
                      :fill         "#abcabc"}))]
      (-> (into
           [:g {:id "waypoint-collection"}]
           (map-indexed
            (fn [idx {:keys [colour position label]}]
              (let [[wx wy] position
                    [ex ey] [100 100]]
                (-> (el/g
                     (-> (el/circle 10)
                         (tf/style {:style        {:pointer-events "auto"}
                                    :fill         colour
                                    :stroke       "black"
                                    :stroke-width 2
                                    :onclick      (fe-send {:dispatch :set-camera
                                                            :position [(- wx (int (/ ex 2)))
                                                                       (- wy (int (/ ey 2)))]})}))
                     (-> (el/text label) (tf/translate [40 1])))
                    (tf/translate [0 (* 30 idx)]))))
            (conj (vals waypoints) {:colour "#811CFBaa" :position [0 0] :label "origin"})))
          (tf/translate [20 60]))
      ;;
      #_(into [:g]
              (map (fn [[x y]]
                     (let [left (* (- x cx) size)
                           top  (* (- y cy) size)]
                       [:rect {:id      (format "location_ref_%s_%s" x y)
                               :width   size
                               :height  size
                               :x       left
                               :y       top
                               :stroke  "purple"
                               :fill    "lavender"
                               :opacity 0.2}])))
              (filter identity #_c/loc? (keys @c/cells)))]
     (when overlay-on
       (map #(render-refs % state) (vals entities))))))

(defn image
  [file-path]
  [:img {:src (format "data:image/png;base64, %s" (encode-image-to-base64 file-path))}])

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
    (catch Exception _e
      (println "Error reading string.")
      nil)))

(defn- render-markdown-string
  [s]
  (-> s md/parse md.transform/->hiccup (assoc 0 :div.markdown)))

(defn- hiccup?
  [value]
  (when (seqable? value)
    (and (vector? value)
         (keyword? (first value)))))

;; display types:
;; content -> show the codemirror editor, hide value
;; note    -> show a markdown render of the content
;; value   -> show the value render, hide editor
;; control -> show a control based on the content. Eg. number = slider or an ^v input boxes
;; none    -> completely hide the cell (not sure what indicator should be left?, maybe just a tiny triangle in top left)

(defn- number-content?
  [content]
  (number? (maybe-read-string content)))

(defn- points-content?
  [content]
  (when-let [v (maybe-read-string content)]
    (and (vector? v)
         (every? #(and
                   (vector? %)
                   (every? number? %)
                   (= 2 (count %))) v))))

(defn- touch-tilt-control?
  [content]
  (if (string? content)
    (when-let [v (maybe-read-string content)]
      (and (map? v)
           (= (:control v) :touch-tilt-control)))
    (and (map? content)
         (= (:control content) :touch-tilt-control))))

(defn- drawing-canvas?
  [content]
  (if (string? content)
    (when-let [v (maybe-read-string content)]
      (and (map? v)
           (= (:control v) :drawing-canvas)))
    (and (map? content)
         (= (:control content) :drawing-canvas))))

(defn estimate-size
  ([data] (estimate-size data 0))
  ([data depth]
   (cond
     (nil? data) 4  ; "nil"
     (number? data) 8  ; Rough estimate for numbers
     (string? data) (count data)
     (keyword? data) (+ 1 (count (name data)))
     (symbol? data) (count (str data))
     (vector? data) (if (< depth 2)
                      (reduce + 2 (map #(estimate-size % (inc depth)) data))
                      (+ 2 (* (count data) 3)))  ; [...]
     (map? data) (if (< depth 2)
                   (reduce + 2 (mapcat #(map (fn [x] (estimate-size x (inc depth))) %) data))
                   (+ 2 (* (count data) 5)))  ; {...}
     (seq? data) (if (< depth 2)
                   (reduce + 1 (map #(estimate-size % (inc depth)) data))
                   (+ 1 (* (count data) 3)))  ; (...)
     :else 10)))  ; Default estimate for other types

(defn render-value
  [value]
  (cond
    (> (estimate-size value) 1000)       "Value too large for display."
    (and (vector? value)
         (not (keyword? (first value)))) (pr-str value)
    (hiccup? value)                      value
    (map? value)                         (with-out-str (pp/pprint value))
    :else                                (str value)))

;; maybe start using kindly here
(defn render-value2
  [{:keys [id value display]}]
  [:span {:id (format "value-%s" id)}
   (case display
     :note (if (string? value)
             (render-markdown-string value)
             (render-value value))
     (render-value value))])

(defn editor
  ([entity state] (editor false entity state))
  ([init? {:keys [display location content id size]} state]
   (let [wrap                   (if init? wrap-js-in-content-loaded identity)
         {global-size :size
          camera      :camera}  state
         enable-editor?         (= display :content)
         {camera-loc :location} camera
         [x y]                  (mapv - location camera-loc)
         [nx ny]                size
         w                      (* nx global-size)
         h                      (* ny global-size)
         left                   (* x global-size)
         top                    (* y global-size)
         editor-style           {:background-color "rgba(255,255,255,0.125);"
                                 :box-sizing       "border-box"
                                 :overflow         "hidden"
                                 :position         "absolute"
                                 :left             left
                                 :top              top
                                 :width            w
                                 :height           h
                                 :z-index          "1000"}]
     [:div
      {:cursor "auto"
       :id     (format "entity%s" id)}
        [:div.resizeable
         {:id    (format "movable%s" id)
          :style (merge
                  editor-style
                  (when enable-editor?
                    {:overflow "visible"
                     :filter   "drop-shadow(0px 2px 2px rgba(9, 9, 10, 0.35))"}))}
         [:div {:style {:height  (* ny global-size)
                        :display (if (#{:none :content :control} display) "none" "block")}}
          (render-value2 {:id      id
                          :value   (c/value id)
                          :display display})]
         [:div {:id    (str id)
                :style {:display (if enable-editor? "block" "none")
                        :height  (* ny global-size)}}
          [:hiccup/raw-html content]]
         [:div
          {:style {:position  "absolute"
                   :top       0
                   :right     0
                   :padding   2
                   :font-size "7pt"}}
          "ID:" id]
         ;; scripts
         [:<>
          (when (= display :control)
            (cond
              (number-content? content)
              [:script ((if init? wrap-js-in-content-loaded identity) (clj->js `(makeNumberInput ~id)))]

              (points-content? content)
              [:points-editor
               {:id          id
                :data-points (json/encode (mapv (fn [[x y]] {:x x :y y}) (maybe-read-string content)))}]

              (touch-tilt-control? content)
              [:touch-tilt-control
               {:style {:display "block"
                        :width   "100%"
                        :height  "100%"}
                :id    id}]

              (drawing-canvas? content)
              [:drawing-canvas
               {:data-points (json/encode (mapv (fn [[x y]] {:x x :y y}) (:pts (maybe-read-string content))))
                :style       {:display "block"
                              :width   "100%"
                              :height  "100%"}
                :id          id}]

              :else nil))
          (when enable-editor?
            [:script (wrap (clj->js `(createEditorInstance ~id)))])
          [:script (wrap (format "attachEntityListeners('movable%s');" id))]]]])))

(def points-editor-template
  [:template#points-editor-template
   [:style     "
    :host {
      width: 100%;
      height: 100%;
      pointer-events: none;
    }
    svg {
      pointer-events: auto;
      box-sizing: border-box;
      width: 100%;
      height: 100%;
      border: 1px solid #000;
    }
    button {
      pointer-events: auto;
    }
    .draggable {
      pointer-events: auto;
      cursor: move;
    }
    path {
      fill: none;
      stroke: black;
      stroke-width: 2;
    }"]
   [:div.prevent-cursor-move
    {:style {:position    "absolute"
             :padding     5
             :white-space "nowrap"}}
    [:button#add-point.prevent-cursor-move "[+]"]
    [:button#remove-point.prevent-cursor-move "[-]"]
    [:button#toggle-mode.prevent-cursor-move "MODE (polyline)"]]
   [:svg.prevent-cursor-move {:xmlns "http://www.w3.org/2000/svg"}
    [:g#keep
     [:circle {:cx 0 :cy 0 :r 7 :fill "green"}]]]])

(defmacro ->js
  [& forms]
  (let [res (squint/compile-string (str/join "\n" forms))]
    res))

(defn cell
  [{:keys [id position size content output latest-output]} state & {:keys [init] :or {init false}}]
  (let [{global-size :size} state
        enable-editor?      true
        [x y]               position
        [nx ny]             size
        display             :content
        w                   (* nx global-size)
        h                   (* ny global-size)
        left                (* x global-size)
        top                 (* y global-size)
        editor-style        {:background-color "rgba(255,255,255,0.125);"
                             :box-sizing       "border-box"
                             :overflow         "hidden"
                             :position         "absolute"
                             :left             left
                             :top              top
                             :width            w
                             :height           h
                             :z-index          "1000"}
        wrap-fn             (if init
                              wrap-js-in-content-loaded
                              identity)]
    [:div
     {:cursor "auto"
      :id     (format "entity%s" id)}
     [:div.resizable
      {:id    (format "movable%s" id)
       :style (merge
               editor-style
               (when enable-editor?
                 {:overflow "visible"
                  #_#_:filter   "drop-shadow(0px 2px 2px rgba(9, 9, 10, 0.35))"}))}
      ;; value render container
      #_[:div {:style {:height  (* ny global-size)
                       :display (if (#{:none :content :control} display) "none" "block")}}
         (render-value2 {:id      id
                         :value   (c/value id)
                         :display display})]
      ;; content string container
      [:div {:id    (str id)
             :style {:display (if enable-editor? "block" "none")
                     :height  (* ny global-size)}}
       [:hiccup/raw-html content]]
      ;; value
      [:div "Last Output: " (render-value latest-output)]
      [:div "---- Output: " (render-value output)]
      ;; ID tag
      [:div
       {:style {:position  "absolute"
                :top       0
                :right     0
                :padding   2
                :font-size "7pt"}}
       "ID:" id]
      [:<>
       [:div.outer-resize-handle.top-left [:div.resize-handle]]
       [:div.outer-resize-handle.top-middle [:div.resize-handle]]
       [:div.outer-resize-handle.top-right [:div.resize-handle]]
       [:div.outer-resize-handle.middle-right [:div.resize-handle]]
       [:div.outer-resize-handle.bottom-right [:div.resize-handle]]
       [:div.outer-resize-handle.bottom-middle [:div.resize-handle]]
       [:div.outer-resize-handle.bottom-left [:div.resize-handle]]
       [:div.outer-resize-handle.middle-left [:div.resize-handle]]]
      ;; scripts
      [:<>
       #_(when true #_(= display :control)
               (cond
                 (number-content? content)
                 [:script (wrap-fn (clj->js `(makeNumberInput ~id)))]

                 (points-content? content)
                 [:points-editor
                  {:id          id
                   :data-points (json/encode (mapv (fn [[x y]] {:x x :y y}) (maybe-read-string content)))}]

                 (touch-tilt-control? content)
                 [:touch-tilt-control
                  {:style {:display "block"
                           :width   "100%"
                           :height  "100%"}
                   :id    id}]

                 (drawing-canvas? content)
                 [:drawing-canvas
                  {:data-points (json/encode (mapv (fn [[x y]] {:x x :y y}) (:pts (maybe-read-string content))))
                   :style       {:display "block"
                                 :width   "100%"
                                 :height  "100%"}
                   :id          id}]

                 :else nil))
       (when enable-editor?
         [:script (wrap-fn (clj->js `(createEditorInstance ~id)))])
       #_[:script (wrap-fn (format "attachEntityListeners('movable%s');" id))]]]]))

(def button-style-map
  {:width         "40px"
   :aspect-ratio  "1/1"
   :box-sizing    "border-box"
   :border        "1px solid #222"
   :border-radius "2px"
   :font-family   "monospace"
   :font-size     "11pt"
   :cursor        "pointer"})

(defn button
  ([label tooltip action] (button {} label tooltip action))
  ([style label tooltip action]
   [:button.prevent-cursor-move
    {:title   tooltip
     :onclick action
     :style   (merge button-style-map style)}
    label]))

(defn button-bar
  [state]
  (let [cursor-pos       (get-in state [:cursor :location])
        button-bar-style {:position        "fixed"
                          :bottom          "20px"
                          :display         "flex"
                          :flex-direction  "row"
                          :gap             "5px"
                          :align-items     "center"
                          :justify-content "center"
                          :width           "100%"
                          :margin          "0 auto"
                          :box-sizing      "border-box"}]
    [:div.prevent-cursor-move {:style button-bar-style}
     (button "⌾" "Return To Home Position." (format "setContainerPosition(%s, %s);" 50 50))
     (button "⌖" "Add/Remove the Waypoint at top-left of the cursor."
             (fe-send {:dispatch :toggle-waypoint :position cursor-pos}))
     #_(button "↑" "Toggle Display Mode." (fe-send {:dispatch :toggle-display :direction :up}))
     #_(button "↓" "Toggle Display Mode." (fe-send {:dispatch :toggle-display :direction :down}))
     [:div "|"]
     (button "⏯" "Process One Step." (fe-send {:dispatch :run-command
                                          :command  :process-one}))
     [:div "|"]
     (button "+" "Add a Cell." (fe-send {:dispatch :add-cell}))
     (button {:font-size "8pt"} "❌" "Delete this Element." (fe-send {:dispatch :remove-cell}))
     [:div {:style {:font-size   "8pt"
                    :width       0
                    :user-select "none"}}
      (str (:display (get-in state [:entities (:active state)])))]]))

(defn cursor
  [{:keys [location size]} {:keys [waypoints] :as state}]
  (let [{camera      :camera
         global-size :size
         active      :active} state
        camera-loc            (:location camera)
        [ox oy]               location
        [x y]                 (mapv - location camera-loc)
        [nx ny]               size
        cursor-width          (* nx global-size)
        cursor-height         (* ny global-size)]
    [:<>
     [:div#insert-target {:hx-swap-oob "afterend"}]
     [:div#cursor.smooth
      {:grid-size         global-size
       :camera-location   (str/join "," (get-in state [:camera :location]))
       :cursor-location   (str/join "," location)
       :cursor-size       (str/join "," size)
       :data-store        (json/encode (:store state))
       :active-element-id active
       :style             {:z-index  "2000"
                           :position "relative"}}
      (let [pos-indicator-str      (format "[%s %s]" ox oy)
            approx-pos-indicator-w (* 0.45 (count pos-indicator-str))]
        [:div {:style {:position    "absolute"
                       :user-select "none"
                       :left        (* (- x approx-pos-indicator-w) global-size)
                       :top         (* (dec y) global-size)}}
         pos-indicator-str])
      (let [pos-indicator-str (format "[%s %s]" (+ ox nx) (+ oy ny))]
        [:div {:style {:position    "absolute"
                       :user-select "none"
                       :left        (* (+ x nx) global-size)
                       :top         (* (+ y ny) global-size)}}
         pos-indicator-str])
      [:div {:style {:box-sizing     "border-box"
                     :border-radius  4
                     :position       "absolute"
                     :pointer-events "none"
                     :margin         -6
                     :left           (* x global-size)
                     :top            (* y global-size)
                     :width          cursor-width
                     :height         cursor-height}}
       (cursor-icon cursor-width cursor-height)]
      (into [:div#waypoints
             (home-point state)]
            (mapv #(waypoint %1 state) (vals waypoints)))]]))
