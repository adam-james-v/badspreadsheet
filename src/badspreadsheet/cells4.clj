(ns badspreadsheet.cells4
  (:require
   [badspreadsheet.machines :as machines]
   [clojure.edn :as edn]
   [clojure.walk :as walk]))

(def cells machines/state)
(defonce cell-counter (atom -1))
(defn new-cell-id [] (swap! cell-counter inc))

(defn cell
  "create a cell."
  [])

(defn reset-cell!
  "Resets the given cell's function and inputs."
  [])

(defn swap-function!
  "Swaps in a new function (of the same arity) to the given cell."
  [id f])

(defn touch!
  "Trigger a recalculation."
  [machine-ref]
  (machines/run-machine (machines/get-machine machine-ref) :force))

(defn remove!
  "Destroy the given cell"
  [machines-or-refs]
  (machines/remove-machines! machines-or-refs))

(defn destroy-cells!
  "Destroy all of the cells"
  []
  (reset! cell-counter -1)
  (reset! cells {:machines {} :grid {}}))

(defn get-cell
  [ref-or-machine]
  (machines/get-machine ref-or-machine))

(defn value
  "get the value of a cell"
  [ref-or-machine]
  (:latest-output (machines/get-machine ref-or-machine)))

(defn c-get
  "Like `get` but for cells."
  [ref-or-machine k]
  (get (machines/get-machine ref-or-machine) k))

(defn c-assoc
  "Like `assoc` but for cells."
  ([ref-or-machine key val] (c-assoc ref-or-machine key val nil nil))
  ([ref-or-machine key val & kvs]
   (let [new-cell (apply (partial assoc (machines/get-machine ref-or-machine)) (concat [key val] (remove nil? kvs)))]
     (swap! cells assoc-in [:machines (:id new-cell)] new-cell))))

(defn c-dissoc
  "Like `dissoc` but for cells."
  ([ref-or-machine key] (c-dissoc ref-or-machine key nil))
  ([ref-or-machine key & ks]
   (let [new-cell (apply (partial dissoc (machines/get-machine ref-or-machine)) (conj ks key))]
     (swap! cells assoc-in [:machines (:id new-cell)] new-cell))))

(defn c-merge
  "Like `merge` but for cells."
  [ref-or-machine & maps]
  (let [new-cell (apply merge (machines/get-machine ref-or-machine) maps)]
    (swap! cells assoc-in [:machines (:id new-cell)] new-cell)))

(defmulti c#
  (fn [k & _args] k))

(defmethod c# :id
  [_ id]
  [id])

(defmethod c# :ids
  [_ ids]
  ids)

(defmethod c# :position
  [_ pos]
  [pos])

(defmethod c# :positions
  [_ positions]
  positions)

(defmethod c# :row
  [_ y]
  (let [[xmin xmax] (:x (machines/grid-extents))
        refs (map (fn [x] [x y]) (range xmin (inc xmax)))]
    refs))

(defmethod c# :col
  [_ x]
  (let [[ymin ymax] (:y (machines/grid-extents))
        refs (map (fn [y] [x y]) (range ymin (inc ymax)))]
    refs))

(defmethod c# :region
  ([_ [x1 y1] w h]
   (machines/window [x1 y1] w h))
  ([_ [x1 y1] [x2 y2]]
   (machines/window [x1 y1] [x2 y2])))

(defmethod c# :neighbours
  ([_ pos] (c# :neighbours [:n :ne :e :se :s :sw :w :nw] pos))
  ([_ cardinal-dirs pos]
   (let [rel-coords (map machines/cardinal-dirs->rel-coords cardinal-dirs)
         refs       (map (fn [v] (mapv + v pos)) rel-coords)]
     (distinct refs))))

(defmethod c# :relative
  [_ rel-coords pos]
  (let [refs (map (fn [v] (mapv + v pos)) rel-coords)]
    (distinct refs)))

(defmethod c# :relative-region
  ([_ [x1 y1] w h pos]
   (let [refs (map (fn [v] (mapv + v pos)) (machines/window [x1 y1] w h))]
     refs))
  ([_ [x1 y1] [x2 y2] pos]
   (let [refs (map (fn [v] (mapv + v pos)) (machines/window [x1 y1] [x2 y2]))]
     refs)))

(defn- get-c#s
  [form]
  (letfn [(collect [x]
            (cond
              (and (list? x) (seq x) (#{'c#} (first x))) [x]
              (coll? x)                                  (mapcat collect x)
              :else                                      []))]
    (vec (distinct (collect form)))))


;; since c#s can map to a list of refs, I need a 2 stage process:
;; ALL refs in a flat arg list in top-most fn wrapping.
;; (fn [:a [0 0] [1 1] ... ] ... )
;; then inside that a let form that remaps the flat set of refs into their expected shape:
;; (let [c#1


;; almost right. 2 problems:
;; 1. if a ref is a single ref, it is wrapped in a vector in the let binding
;; 2. the c# usage inside refs needs to conditionally plug in the position so that it'll actually work.
(def ^:private relative-ref
  #{:neighbours :relative :relative-region})

(defn- contains-symbol?
  [form]
  (let [symbols (atom false)]
    (walk/postwalk
     (fn [x]
       (when (symbol? x)
         (reset! symbols true)))
     form)
    @symbols))

(defn process-form
  [pos form]
  (let [c#s (get-c#s form)]
    (if (seq c#s)
      (let [let-syms    (vec (repeatedly (count c#s) #(gensym "c#")))
            let-smap    (zipmap c#s let-syms)
            refs        (map
                         (fn get-refs
                           [let-sym [_ & args]]
                           (if (relative-ref (first args))
                             [let-sym (apply c# (concat args [pos]))]
                             [let-sym (apply c# args)]))
                         let-syms c#s)
            bindings    (mapv
                         (fn make-let
                           [[sym ref]]
                           (let [refsym (repeatedly (count ref) #(gensym "ref-"))]
                             [sym (if (= 1 (count ref))
                                    (first refsym)
                                    (vec refsym))]))
                         refs)
            fn-syms     (vec (apply concat (map (fn [[_ syms]] (if (symbol? syms) [syms] syms)) bindings)))
            let-binding (vec (apply concat bindings))
            [do? & wrapped-inner
             :as inner] (walk/postwalk-replace  let-smap form)
            inner-form  (if (= do? 'do)
                          wrapped-inner
                          [inner])
            _           (println inner-form)]
        {:form           form
         :processed-form `(fn ~fn-syms
                            (let ~let-binding
                              ~@inner-form))
         :refs           (vec (mapcat second refs))})
      (let [[do? & wrapped-inner
             :as inner] form
            inner-form  (if (= do? 'do)
                          wrapped-inner
                          [inner])]
        {:form           form
         :processed-form (if (contains-symbol? inner-form)
                           inner-form
                           `(fn [] ~@inner-form))}))))

(defn formula
  "Given a form string, produce a working cell. Returns the newly created cell's ID."
  ([position form-or-str] (formula position [2 4] form-or-str))
  ([position size form-or-str]
   (let [form                          (if (string? form-or-str)
                                         (edn/read-string (format "(do %s)" form-or-str))
                                         form-or-str)
         {:keys [processed-form refs]} (process-form position form)
         f                             (eval processed-form)
         id                            (get-in @cells [:grid position] (new-cell-id))
         size                          (get-in @cells [:machines id :size] size)]
     (machines/add-machine! id position size refs f)
     (c-assoc id :content (if (string? form-or-str)
                            form-or-str
                            (str form-or-str)))
     id)))
