(ns badspreadsheet.cells3
  (:require
   [missionary.core :as m]))

;; from the readme:
#_
(comment
  (def !input (atom 1))

  (def main
    (let [<x (m/signal (m/watch !input))
          <y (m/signal (m/latest + <x <x))]
      (m/reduce (fn [_ x] (prn x)) nil <y)))

  (def dispose!
    (main
     #(prn ::success %)
     #(prn ::crash %)))

  (defn trivial-watch
    [to-watch]
    (let [f (m/reduce (fn [_ x] (prn x)) nil (m/signal (m/watch to-watch)))]
      (f #(prn ::success %)
         #(prn ::crash %))))


  ;; Ok, let's try to think through how this might work, before worrying about matching the api of cells2, which is pretty reasonably the api I want.

  ;; If I look at the def main above, I can sort of treat the (m/latest + <x <x) as the function application
  ;; inside any given cell like this: (apply m/latest (concat f inputs)) (or perhaps this will work, if f accepts & args: (m/latest f args)?

  ;; for now, let's assume the inputs are a vec of atoms, which we can apply signal/watches to

  (defonce ca (atom {:value 20}))
  (defonce cb (atom {:value 20}))

  (defn ->signal
    [ref]
    (m/signal (m/watch ref)))

  (defn try-f
    [f]
    (fn [& args]
      (try
        (apply f args)
        (catch Exception e
          {:error (ex-message e)}))))

  (defn simple-cell
    [f inputs]
    (let [c-state (atom {:value nil})
          <inputs (map ->signal inputs)
          <output (m/signal (apply m/latest (concat [(try-f f)] <inputs)))
          f       (m/reduce (fn [_ x] (swap! c-state assoc :value x)) nil <output)]
      (swap! c-state assoc :dispose! (f #(prn ::success %)
                                        #(prn ::crash %)))
      c-state))

  (defonce cc (simple-cell (fn [a b] (* (:value a) (:value b))) [ca cb]))

  ;; Ok, so that sort of works the way I want!
  ;; One thing to be careful about is in the repl, re-defing over a cell without calling dispose! will keep its watches running
  ;; until you intentionally break it.
  ;; BUT, since I wrap the fns in try-f... it will kind of keep going, so that's not good for real-world.

  ;; Now we can work on matching the api I need.
  ;; if I reset-cell!, the following needs to happen:
  ;; 1. dispose! the current m/reduce f
  ;; 2. rebuild the <inputs
  ;; 3. rebuild the <output
  ;; 4. rebuild f
  ;; 5. swap! all of the above into the c-state (in one update!) -> this action should trigger any listening cells

  (defn reset-simple-function!
    [c-state f inputs]
    ((:dispose! @c-state))
    (let [<inputs (map ->signal inputs)
          <output (m/signal (apply m/latest (concat [(try-f f)] <inputs)))
          f       (m/reduce (fn [_ x] (swap! c-state assoc :value x)) nil <output)]
      (swap! c-state assoc :dispose! (f #(prn ::success %)
                                        #(prn ::crash %)))
      c-state))

  ;; Alright, that basically works just fine!
  ;; Now, I don't want to have to write my input functions to pull :value out of the cell state, so I should probably make a little fn wrapper that does it on my behalf.

  (defn f*
    [f]
    (fn [& args]
      (apply f (map :value args))))

  (defn simple-cell2
    [f inputs]
    (let [c-state (atom {:value nil})
          <inputs (map ->signal inputs)
          <output (m/signal (apply m/latest (concat [(try-f (f* f))] <inputs)))
          f       (m/reduce (fn [_ x] (swap! c-state assoc :value x)) nil <output)]
      (swap! c-state assoc :dispose! (f #(prn ::success %)
                                        #(prn ::crash %)))
      c-state)))



;; The necessary elements of a cells implementation:
;; - defn cell
;; - defn formula
;; - swap-function!
;; - reset-function!
;; - touch!
;; - value
;; - c-get/assoc/merge/dissoc
;; - destroy!
;; - some way to hook into all cell changes

(defonce global-cells (atom {}))
(defonce cell-counter (atom -1))
(defn new-cell-id [] (swap! cell-counter inc))

;; what to do if you make a cell that has a cell-id that doesn't exist yet?
;; the cell shouldn't fail... and should have a way to 'come online' properly when the cell is created.
;; likewise, if a cell does exist but is later destroyed... the cell should also not break.
;; I'll try to figure that out in a minute, let's get the basics working.
;; Idea: If not all inputs exist as cells, treat the cell as a value cell (since it can't compute without all inputs available anyway)?

(defn ->signal
  [ref]
  (m/signal (m/watch ref)))

(defn try-f
  [f]
  (fn [& args]
    (try
      (apply f args)
      (catch Exception e
        {:error (ex-message e)}))))

(defn f*
  [f]
  (fn [& args]
    (apply f (map :value args))))

(defn- get-cell
  [id-or-cell]
  (when-not (#{::touch-key} id-or-cell)
    (if (instance? clojure.lang.Atom id-or-cell)
      id-or-cell
      (get @global-cells id-or-cell))))

(defn- covers
  [{:keys [location size]}]
  (when (and location size)
     (set
      (let [[x y]   location
            [nx ny] size]
        (for [sx (range nx)
              sy (range ny)]
         [(+ x sx) (+ y sy)])))))

(defn- cell-contains-location?
  [cell-map loc]
  (contains? (covers cell-map) loc))

(defn get-cell-by-location
  [loc]
  (let [cells (filter #(instance? clojure.lang.Atom %) (vals @global-cells))]
    (first (filter #(cell-contains-location? (deref %) loc) cells))))

(defonce location-touch (atom {}))

(defn- location-proxy-cell
  [loc]
  (let [c-state (atom {:value nil})
        fp      (fn [_ _]
                  (let [c (get-cell-by-location loc)]
                    (if c (:value @c) "")))
        <inputs (mapv ->signal [global-cells location-touch])
        <output (m/signal (apply m/latest (concat [fp] <inputs)))
        f       (m/reduce (fn [_ x]
                            (swap! c-state assoc :value x)) nil <output)]
    (swap! c-state assoc :dispose! (f #(prn ::success %)
                                      #(prn ::crash %)))
    (swap! global-cells assoc loc c-state)
    c-state))

(defn- value-cell*
  [c-state]
  (let [{:keys [id cell-fn value]} @c-state]
    ;; make the dispose function accessible from the state
    (swap! c-state merge
           {:dispose! (fn [] nil)
            :value (or ((try-f cell-fn)) value)})
    ;; register the cell (the c-state atom) in the global-cells atom
    (swap! global-cells assoc id c-state)
    c-state))

(defn- value-cell
  "A cell that only holds a value, it does not need to react to anything!"
  [id cell-fn initial-output]
  (let [cell-map {:id      id
                  :inputs  []
                  :cell-fn cell-fn
                  :value   (or ((try-f cell-fn)) initial-output)}
        c-state  (atom cell-map)]
    (value-cell* c-state)))

(defn- function-cell*
  [c-state input-cell-ids inputs]
  (let [{:keys [id cell-fn]} @c-state
        <inputs              (map ->signal inputs)
        <output              (m/signal (apply m/latest (concat [(try-f (f* cell-fn))] <inputs)))
        f                    (m/reduce (fn [_ x] (swap! c-state assoc :value x)) nil <output)]
    ;; make the dispose function accessible from the state
    (swap! c-state assoc
           :dispose! (f #(prn ::success %)
                        #(prn ::disposed %))
           :inputs input-cell-ids)
    ;; register the cell (the c-state atom) in the global-cells atom
    (swap! global-cells assoc id c-state)
    c-state))

(defn- function-cell
  "A cell that reacts to changes on its inputs."
  [id input-cell-ids cell-fn initial-output]
  (let [inputs   (mapv #(get @global-cells %) input-cell-ids)
        cell-map {:id      id
                  :inputs  input-cell-ids
                  :cell-fn cell-fn
                  :value   initial-output}
        c-state  (atom cell-map)]
    (function-cell* c-state input-cell-ids inputs)))

(defn cell
  "create a cell."
  [id input-cell-ids cell-fn initial-output]
  (let [inputs (mapv #(get @global-cells %) input-cell-ids)]
    (if (and
         (seq inputs)
         (every? some? inputs))
      (function-cell id input-cell-ids cell-fn initial-output)
      (value-cell id cell-fn initial-output))))

#_
(defn reset-cell!
  "Resets everything about the given cell."
  [id-or-cell f input-cell-ids]
  (let [c      (get-cell id-or-cell)
        inputs (mapv #(get @global-cells %) input-cell-ids)]
    ((:dispose! @c))
    (swap! c assoc :cell-fn f)
    (if (and
         (seq inputs)
         (every? some? inputs))
      (function-cell* c input-cell-ids inputs)
      (value-cell* c))))

(defn loc?
  [l]
  (and (vector? l)
       (= 2 (count l))
       (every? integer? l)))

#_
(defn reset-cell!
  "Resets everything about the given cell."
  [id-or-cell f input-cell-ids]
  (let [c      (get-cell id-or-cell)
        _      (doseq [loc (filter loc? input-cell-ids)]
                 (let [c (get @global-cells loc)]
                   (when-not c
                     (location-proxy-cell loc))))
        inputs (mapv (fn [id-or-loc] (get @global-cells id-or-loc)) input-cell-ids)]
    ((:dispose! @c))
    (swap! c assoc :cell-fn f)
    (if (and
         (seq inputs)
         (every? some? inputs))
      (function-cell* c input-cell-ids inputs)
      (value-cell* c))))

(defn reset-cell!
  "Resets everything about the given cell."
  [id-or-cell f input-cell-ids]
  (let [c      (get-cell id-or-cell)
        inputs (mapv (fn [id-or-loc]
                       (if (loc? id-or-loc)
                         (get @global-cells id-or-loc (location-proxy-cell id-or-loc))
                         (get @global-cells id-or-loc))) input-cell-ids)]
    ((:dispose! @c))
    (swap! c assoc :cell-fn f)
    (if (and
         (seq inputs)
         (every? some? inputs))
      (function-cell* c input-cell-ids inputs)
      (value-cell* c))))

(defn swap-function!
  [id f]
  (reset-cell! id f []))

(defn reset-function!
  [id f inputs]
  (reset-cell! id f inputs))

;; maybe I don't need this capability with this new design?
(defn touch!
  "Cause the cell to output by changing its state with an arbitrary change."
  [id-or-cell]
  (when-let [c (get-cell id-or-cell)]
    (swap! c update ::touch-key not)
    (swap! global-cells update ::touch-key not)))

(defn destroy!
  "Destroy the cell, making sure to dispose of the missionary flow."
  [id-or-cell]
  (when-let [c (get-cell id-or-cell)]
    (let [id (:id @c)]
      ((or (:dispose! @c) (fn [] nil)))
      (reset! c nil)
      (swap! global-cells dissoc id)
      (println "destroyed: " id))))

(defn destroy-cells!
  []
  (doseq [id (keys @global-cells)]
    (destroy! id))
  (reset! cell-counter -1)
  (reset! global-cells {}))

(defn value
  [id-or-cell]
  (let [c (get-cell id-or-cell)]
    (when c
      (:value @c))))

(defn c-get
  [id-or-cell k]
  (let [c (get-cell id-or-cell)]
    (get @c k)))

(defn c-assoc
  [id-or-cell k v]
  (let [c (get-cell id-or-cell)]
    (swap! c assoc k v)))

(defn c-merge
  [id-or-cell m]
  (let [c (get-cell id-or-cell)]
    (swap! c merge m)))

(defn c-dissoc
  [id-or-cell k]
  (let [c (get-cell id-or-cell)]
    (swap! c dissoc k)))

(defn- cell-exists?
  [id]
  (contains? @global-cells id))

(defn formula
  "Make a new global cell. Returns the new cell's cellID."
  ([f] (formula (new-cell-id) f []))
  ([f inputs] (formula (new-cell-id) f inputs))
  ([id f inputs]
   (if (cell-exists? id)
     (reset-function! id f inputs)
     (cell id inputs f inputs))
   id))
