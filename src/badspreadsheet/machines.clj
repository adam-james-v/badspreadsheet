(ns badspreadsheet.machines
  (:require
   [clojure.core.async :as async]
   [clojure.set :as set])
  (:import
   (java.util.concurrent Executors Future)))

;; an experiment where 'cells' are treated more like machines in a factory game.

(defrecord Machine [id position size sources inputs output latest-output operation memoized-operation])

(defonce state (atom {:machines {}
                      :grid {}}))

;; types of ref:
;; location: [x y]
;; ID: int ID perhaps also key ID, for user-generated machines (eg via repl)
;; more complex ref methods should be handled with additional functions
;; that call get-machine with locations or IDs. eg. get-machines with a window function
(defn get-machine
  ([ref] (get-machine @state ref))
  ([state ref]
   (or
    ;; ref is a machine ID
    (get-in state [:machines ref])
    ;; ref is a position
    (get-in state [:machines (get-in state [:grid ref])])
    ;; ref is a machine already, still want to get the machine from state by same ID
    (get-in state [:machines (:id ref)]))))

(defn window
  ([[x y] w h] (window [x y] [(+ x w) (+ y h)]))
  ([[x1 y1] [x2 y2]]
   (let [[x1 x2] (sort [x1 x2])
         [y1 y2] (sort [y1 y2])]
     (for [x (range x1 (inc x2))
           y (range y1 (inc y2))]
       [x y]))))

(defn- machine-id->positions
  [{:keys [grid]} machine-id]
  (keep
   (fn [[pos id]]
     (when (= id machine-id)
       pos))
   grid))

(defn- update-grid
  [state]
  (reduce
   (fn a [acc-state {:keys [id position size] :as _machine}]
     (let [[w h] size
           new-positions (set (window position w h))
           old-positions (set (machine-id->positions acc-state id))]
       (if (not= new-positions old-positions)
         (-> acc-state
             (update :grid (fn [grid] (apply dissoc grid old-positions)))
             (update :grid (fn [grid] (merge grid (zipmap new-positions (repeat id))))))
         acc-state)))
   state
   (vals (:machines state))))

(defn- copy-outputs-to-inputs
  [state]
  (reduce
   (fn [acc-state machine]
     (if-let [sources (:sources machine)]
       (let [new-inputs (reduce
                         (fn [inputs source-ref]
                           (if-let [source (get-machine acc-state source-ref)]
                             (assoc inputs source-ref (:output source))
                             inputs))
                         (:inputs machine)
                         sources)]
         (assoc-in acc-state [:machines (:id machine) :inputs] new-inputs))
       acc-state))
   state
   (vals (:machines state))))

(defn- clear-all-outputs
  [state]
  (reduce
   (fn [acc-state [id _machine]]
     (-> acc-state
         (assoc-in [:machines id :output] nil)))
   state
   (:machines state)))

(defn- clear-all-inputs
  [state]
  (reduce
   (fn [acc-state [id _machine]]
     (-> acc-state
         (assoc-in [:machines id :inputs] {})))
   state
   (:machines state)))

(defn run-machine
  ([machine] (run-machine machine false))
  ([{:keys [id inputs sources operation memoized-operation] :as machine} force?]
   (if sources
     ;; regular machine that requires some inputs
     (when (every? some? (map #(get inputs %) sources))
       (try
         (let [op     (if force? operation memoized-operation)
               result (apply op (vals inputs))]
           (-> machine
               (assoc :output result)
               (assoc :latest-output result)))
         (catch Exception e
           (println "Error in machine:" id ":" (.getMessage e))
           machine)))
     ;; generator that always runs without inputs
     (try
       (let [op     (if force? operation memoized-operation)
             result (op)]
         (-> machine
             (assoc :output result)
             (assoc :latest-output result)))
       (catch Exception e
         (println "Error in machine:" id ":" (.getMessage e))
         machine)))))

(defn- run-all-machines
  ([state] (run-all-machines state false))
  ([state force?]
   (reduce
    (fn [acc-state [id machine]]
      (if-let [updated-machine (run-machine machine force?)]
        (assoc-in acc-state [:machines id] updated-machine)
        acc-state))
    state
    (:machines state))))

(defn- process-cycle
  [state]
  (-> state
      copy-outputs-to-inputs
      clear-all-outputs
      run-all-machines
      clear-all-inputs
      update-grid))

(defn- process
  [state]
  (let [max-iters (* 2 (count (:machines state)))
        iters (iterate process-cycle state)]
    (or
     (->> iters
          (partition 2 1)
          (take-while (fn [[prev current]] (not= prev current)))
          (take max-iters)
          last
          second)
     (first iters))))

(defn process-one!
  []
  (swap! state
         (fn [s]
           (-> s
               copy-outputs-to-inputs
               clear-all-outputs
               (run-all-machines :force)
               clear-all-inputs
               update-grid))))

(defn- placement-allowed?
  [{:keys [grid] :as _state} id position]
  (or (not (contains? grid position))
      (= (get grid position) id)))

(defn add-machine
  ([state {:keys [id position size sources operation initial-output] :as _machine-def}]
   (add-machine state id position size sources operation initial-output))
  ([state id position size sources operation]
   (add-machine state id position size sources operation nil))
  ([state id position size sources operation initial-output]
   (when (placement-allowed? state id position)
     (let [machine   (->Machine id position size sources {} initial-output nil operation (memoize operation))
           old-pos   (get-in state [:machines id :position])
           new-state (cond->
                         (merge-with merge
                                     state
                                     {:machines {id machine}
                                      :grid     {position id}})
                         (not= old-pos position)
                         (update :grid dissoc old-pos))]
       new-state))))

(defn add-machines
  [state machine-defs]
  (reduce
   (fn [state machine-def]
     (add-machine state machine-def))
   state
   machine-defs))

(defn add-machine!
  ([{:keys [id position size sources operation initial-output] :as _machine-def}]
   (add-machine! id position size sources operation initial-output))
  ([id position size sources operation] (add-machine! id position size sources operation nil))
  ([id position size sources operation initial-output]
   (let [new-state (add-machine @state id position size sources operation initial-output)]
     (swap! state (comp process (fn [s] (merge s new-state)))))))

(defn add-machines!
  [machine-defs]
  (let [new-state (add-machines @state machine-defs)]
    (swap! state (comp process (fn [s] (merge s new-state))))))

(defn remove-machine!
  [machine-ref]
  (let [id                 (or (:id machine-ref)
                               (if (vector? machine-ref)
                                 (get-in @state [:grid machine-ref])
                                 machine-ref))
        {:keys [position size]} (get-in @state [:machines id])
        [w h] size]
    (when position
      (swap! state (fn [s]
                     (-> s
                         (update :machines dissoc id)
                         (update :grid (fn [grid] (apply dissoc grid (window position w h))))))))))

(defn remove-machines!
  [machine-refs]
  (let [ids       (->> machine-refs
                       (map (fn [machine-ref]
                              (if (vector? machine-ref)
                                (get-in @state [:grid machine-ref])
                                machine-ref)))
                       distinct)
        positions (map
                   (fn [id]
                     (:position (get-in @state [:machines id])))
                   ids)]
    (println "IDS" ids "Positions" positions)
    (when (seq positions)
      (swap! state
             (fn [s]
               (-> s
                   (update :machines (fn [machines] (apply dissoc machines ids)))
                   (update :grid (fn [grid] (apply dissoc grid positions)))))))))

(defn move-machine!
  [machine-ref new-pos]
  (let [{:keys [position id]} (get-machine machine-ref)]
    (when (placement-allowed? @state id new-pos)
      (swap! state (fn [s]
                     (-> s
                         (assoc-in [:machines id :position] new-pos)
                         (assoc-in [:grid new-pos] id)
                         (update :grid dissoc position)
                         process))))))

(defn- print-machines
  [{:keys [machines] :as _state}]
  (doseq [{:keys [id position latest-output]} (vals machines)]
    (println "MACHINE:" id "AT:" position "----->" latest-output)))

(defn print-grid [grid-map]
  (let [min-width   5
        col-widths  (-> (group-by (comp first first) grid-map)
                        (update-vals (comp
                                      #(apply max %)
                                      #(mapv (comp count str last) %))))
        cell-format (fn cell-format
                      ([col] (cell-format col (format ":%s:" col)))
                      ([col val]
                       (format (str "|%" (inc (max
                                               min-width
                                               (get col-widths col min-width))) "s") val)))
        row-line    (fn []
                      (doseq [col (range 10)]
                        (let [value (apply str (repeat (inc (max
                                                             min-width
                                                             (get col-widths col min-width))) "-"))]
                          (print (cell-format col value))))
                      (print "|")
                      (println))]
    ;; Print column headers
    (row-line)
    (doseq [col (range 10)]
      (print (cell-format col)))
    (print "|")
    (println)
    (row-line)

    ;; Print rows
    (doseq [row (range 10)]
      #_(print (cell-format row))       ; Row header
      (doseq [col (range 10)]
        (let [value (get grid-map [col row] "")]
          (print (cell-format col value))))
      (print "|")
      (println)
      (row-line))))

(defn print-machines-on-grid
  [{:keys [machines]}]
  (print-grid
   (into
    {}
    (map (fn [{:keys [latest-output position]}]
           [position latest-output])
         (vals machines)))))

#_(add-watch state :print (fn [_ _ _ new-state] (print-machines-on-grid new-state)))

(defn value
  [machine-ref]
  (let [id (get-in @state [:grid machine-ref] machine-ref)]
    (get-in @state [:machines id :latest-output])))

;; multiple ways to get sets of machines
;; by row -> [:row y]
;; by col -> [:row x]
;; neighbours -> [:neighbours machine] or [:neighbours [:n :ne :e :se :s :sw :w :nw] machine]
;; relative   -> [:relative [list of 'move' vecs] machine]
;; by region  -> [:region [x1 y1] [x2 y2]]

(defn grid-extents
  []
  (let [positions (keys (:grid @state))
        xs (apply (juxt min max) (mapv first positions))
        ys (apply (juxt min max) (mapv second positions))]
    {:x xs
     :y ys}))

(defmulti get-machines
  (fn [k & _args] k))

(defmethod get-machines :row
  [_ y]
  (let [[xmin xmax] (:x (grid-extents))
        refs (map (fn [x] [x y]) (range xmin (inc xmax)))]
    (keep get-machine refs)))

(defmethod get-machines :col
  [_ x]
  (let [[ymin ymax] (:y (grid-extents))
        refs (map (fn [y] [x y]) (range ymin (inc ymax)))]
    (keep get-machine refs)))

(defmethod get-machines :region
  ([_ [x1 y1] w h]
   (keep get-machine (window [x1 y1] w h)))
  ([_ [x1 y1] [x2 y2]]
   (keep get-machine (window [x1 y1] [x2 y2]))))

(def cardinal-dirs->rel-coords
  {:n  [ 0 -1]
   :ne [ 1 -1]
   :e  [ 1  0]
   :se [ 1  1]
   :s  [ 0  1]
   :sw [-1  1]
   :w  [-1  0]
   :nw [-1 -1]})

(defmethod get-machines :neighbours
  ([_ machine] (get-machines :neighbours [:n :ne :e :se :s :sw :w :nw] machine))
  ([_ cardinal-dirs machine]
   (let [rel-coords (map cardinal-dirs->rel-coords cardinal-dirs)
         refs       (map (fn [v] (mapv + v (:position machine))) rel-coords)]
     (distinct (keep get-machine refs)))))

(defmethod get-machines :relative
  [_ rel-coords machine]
  (let [refs (map (fn [v] (mapv + v (:position machine))) rel-coords)]
    (distinct (keep get-machine refs))))

(defmethod get-machines :relative-region
  ([_ [x1 y1] w h machine]
   (let [refs (map (fn [v] (mapv + v (:position machine))) (window [x1 y1] w h))]
     (keep get-machine refs)))
  ([_ [x1 y1] [x2 y2] machine]
   (let [refs (map (fn [v] (mapv + v (:position machine))) (window [x1 y1] [x2 y2]))]
     (keep get-machine refs))))

(comment

  (add-machine! :a [0 0] nil (fn [] (println "A") 1))
  (add-machine! :b [1 0] [[0 0]] (fn [x] (println "B") (* 2 x)))
  (add-machine! :c [0 1] [[0 0]] (fn [x] (println "C") (* 4 x)))
  (add-machine! :d [0 1] [:b :c] (fn [x y] (println "D") (* x y)))
  (add-machine! :e [2 1] nil (fn [] (println "E") -1))
  (add-machine! :f [0 1] [:a :e] (fn [x y] (println "F") (* x y)))
  (add-machine! :g [10 10] [[2 1]] (fn [x] (println "GG") (+ x x)) 0.3)



  )
