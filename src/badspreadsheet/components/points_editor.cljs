
(ns badspreadsheet.components.points-editor
  {:clj-kondo/ignore true}
  (:require
   [squint.core :refer [defclass]]))

(defclass PointsEditorB
  (extends js/HTMLElement)

  ;; fields are things attached to 'this' in the constructor.
  ;; I think const just has to be declared with field too?
  (field template (-> document
                      (.getElementById "points-editor-template")
                      .-content
                      (.cloneNode true)))
  (field svg (-> this .-shadowRoot (.querySelector "svg")))

  (field points [])
  (field mode "polyline")
  (field isPanning false)
  (field startPan {:x 0 :y 0})

  (constructor
   [this]
   (super)

   (.attachShadow this {:mode :open})
   (.initEventListeners this))

  (connectedCallback
   [this]
   ;; get/set the points
   (if (not (.hasAttribute this "data-points"))
     (.setAttribute this "data-points" [])
     (let [pts (.parse JSON (.getAttribute this "data-points"))]
       (.updatePoints this pts)))

   (.updateViewBoxSize this)
   (.addEventListener window "resize" (-> this. .-updateViewBoxSize (.bind this)))
   (.addEventListener svg "mousedown" (-> this .-handleMouseDown (.bind this)))
   (.addEventListener window "mouseup" (-> this .-handleMouseUp (.bind this)))
   (.addEventListener window "mousemove" (-> this .-handleMouseMove (.bind this))))

  (disconnectedCallback
   [this]
   (.removeEventListener window "resize" (-> this .-updateViewBoxSize (.bind this)))
   (.removeEventListener svg "mousedown" (-> this .-handleMouseDown (.bind this)))
   (.removeEventListener window "mouseup" (-> this .-handleMouseUp (.bind this)))
   (.removeEventListener window "mousemove" (-> this .-handleMouseMove (.bind this))))

  Object
  (initEventListeners
   []
   (-> shadowRoot
       (.getElementById "add-point")
       (.addEventListener "click" (fn [] (.addPoint this))))
   (-> shadowRoot
       (.getElementById "remove-point")
       (.addEventListener "click" (fn [] (.removePoint this)))))

  Object
  (handleMouseDown
   [e]
   (.preventDefault e)
   (when (= (.target e) (.svg this))
     (set! isPanning true)
     (set! startPan.x (.clientX e))
     (set! startPan.y (.clientY e))))




  )
