(ns badspreadsheet.cells2
  (:require
   [clojure.set :as set]
   [clojure.core.async :as a
    :refer [chan go-loop
            pub sub unsub
            >! <! >!! <!!]]))

(defn maybe-apply
  [f args]
  (let [args (remove #{::empty} args)]
    (try
      (apply f args)
      (catch Exception e nil))))

(defonce event-bus (chan (a/sliding-buffer 1)))
(defonce event-publisher (pub event-bus :cell-id))
(defonce cell-counter (atom -1))
(defonce global-cells (atom {:cells {}}))

(defn- new-cell-id
  []
  {:id (swap! cell-counter inc)})

(defn- input-vec->map
  [inputs]
  (->> inputs
       (map-indexed (fn [idx v] (or v idx)))
       vec
       set/map-invert))

(defn- connect-inputs
  [inputs channel]
  ;; for all inputs, sub to that input-id
  (doseq [id inputs]
    (when id
      (sub event-publisher id channel))))

(defn- disconnect-inputs
  [inputs channel]
  (doseq [id inputs]
    (when id
      (unsub event-publisher id channel))))

(defn- cell*
  [{:keys [inputs channel initial-output cell-id] :as cell-map}]
  (let [imap (input-vec->map inputs)
        state (atom (assoc cell-map :imap imap))]
    ;; sub the cell's chan to its own cell-id
    ;; this is to listen for :action messages
    ;; :value messages for the cell-id are filtered out (otherwise it'll trigger an endless loop)
    (sub event-publisher cell-id channel)
    (connect-inputs inputs channel)

    (go-loop []
      (let [{id     :cell-id
             value  :value
             action :action} (<! channel)]
        ;; when this cell receives an action, perform it
        (when (and action (= id cell-id))
          (case action
            :stop
            (do
              (swap! state assoc :control :stop)
              (a/close! channel)
              (reset! state nil))

            :touch
            (let [{:keys [process-fn current]} @state
                  v                            (maybe-apply process-fn current)]
              (when v
                (swap! state assoc :output v)
                (>! event-bus {:cell-id cell-id
                               :value   v})))))
        ;; when this cell receives a value, make sure its not itself!
        (when (and value (not= id cell-id))
          (let [{:keys [imap]} @state]
            ;; store the input's value
            (swap! state assoc-in [:current (get imap id)] value))
          ;; maybe compute new output value
          (let [{:keys [process-fn previous current]} @state]
            (cond
              (and process-fn initial-output)
              (when (not= previous current)
                (swap! state assoc :previous current)
                (when-let [v (maybe-apply process-fn current)]
                  (swap! state assoc :output v)))

              process-fn
              (when-let [v (maybe-apply process-fn current)]
                (swap! state assoc :output v))))
          ;; output a value (cached or computed) onto the event bus
          (when-let [v (or (:output @state) initial-output)]
            (>! event-bus {:cell-id cell-id
                           :value   v})))
        (when-not (#{:stop} (:control @state))
          (recur))))
    ;; return the state object so that the cell can be modified
    state))

(defn cell
  [cell-id inputs process-fn initial-output]
  (let [inputs   (vec inputs)
        init-vals (vec (repeat (count inputs) nil))
        cell-map {:cell-id       cell-id
                  :inputs        inputs
                  :channel       (chan (a/sliding-buffer 1))
                  :process-fn    process-fn
                  :inital-output initial-output
                  :previous      init-vals
                  :current       init-vals
                  :output        initial-output}]
    (cell* cell-map)))

(defn stop!
  [cell-id]
  (>!! event-bus {:cell-id cell-id :action :stop}))

(defn touch!
  "Cause the cell to output."
  [cell-id]
  (>!! event-bus {:cell-id cell-id :action :touch}))

(defn destroy!
  "Destroy the cell, making sure to close the chans and stop the go loops."
  [cell-id]
  (println "destroyed: " cell-id)
  (stop! cell-id)
  (swap! global-cells update-in [:cells] dissoc cell-id))

(defn formula
  "Make a new global cell. Returns the new cell's cellID."
  [f & inputs]
  (let [cell-id (new-cell-id)
        c (cell cell-id inputs f nil)]
    (swap! global-cells assoc-in [:cells cell-id] c)
    (touch! cell-id)
    cell-id))

(defn value
  [cell-id]
  (let [c (get-in @global-cells [:cells cell-id])]
    (:output @c)))

(defn swap-function!
  [cell-id f]
  (let [cell-state (get-in @global-cells [:cells cell-id])]
    (swap! cell-state assoc :process-fn f)
    (touch! cell-id)
    cell-id))

(defn reset-function!
  [cell-id f & inputs]
  (let [cell-state (get-in @global-cells [:cells cell-id])]
    (swap! cell-state
           (fn [{channel    :channel
                 old-inputs :inputs
                 :as        state}]
             ;; unsub all old inputs
             ;; sub to new inputs
             ;; swap in the new fn
             (disconnect-inputs old-inputs channel)
             (connect-inputs inputs channel)
             (-> state
                 (assoc :imap (input-vec->map inputs))
                 (assoc :inputs (vec inputs))
                 (assoc :process-fn f))))
    (touch! cell-id)
    cell-id))

(comment

  (def acell (formula (fn [] (* 2 3))))
  (formula (fn [a] (println "printed: " a)) acell)



  )
