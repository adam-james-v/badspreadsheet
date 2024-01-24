(ns badspreadsheet.dataflow
  (:require
   [clojure.set :as set]
   [clojure.core.async :as a
    :refer [chan go go-loop
            pub sub
            >! <! >!! <!!]])
  (:import
   (clojure.lang IDeref IFn)))

#_(defrecord Cell [fn arity defaults inputs-chan output-chan value]
  IDeref
  (deref [cell]
    ;; The deref implementation.
    @(:value cell))
  IFn
  (invoke [cell & args]
    ;; The function invocation implementation.
    (apply (:fn cell) args)))


;; Since I know cells will need input and output channels,
;; And I want to have them only ever hold the latest value, I should learn how channels work.

;; Create two input channels
#_(defonce input-channel-1 (chan (a/sliding-buffer 1)))
#_(defonce input-channel-2 (chan (a/sliding-buffer 1)))

;; Function to create a publisher for a channel
(defn create-publisher [channel]
  (pub channel (constantly :value)))

#_(def input-channel-publisher
  (create-publisher input-channel-1))

#_(def mystate (atom []))

;; Helper function to subscribe to channels and run a function
#_(defn subscribe-and-run [inputs process-fn]
  (let [initial-input-vals (vec (repeat (count inputs) nil))
        state (atom initial-input-vals)
        output (chan (a/sliding-buffer 1))
        output-sub (create-publisher output)]
    (doseq [[idx input] (map-indexed vector inputs)]
      (let [sub-channel (chan)]
        (sub input :value sub-channel)
        (go-loop []
          (when-let [value (<! sub-channel)]
            (swap! state assoc idx value)
            (when (every? some? @state)
              (let [v (apply process-fn @state)]
                (when v (>! output v)))
              #_(reset! state (vec (repeat (count channels) nil))))
            (recur)))))
    output-sub))

;; Example processing function
#_(defn process-inputs [input-1 input-2]
  (let [v (format "Processing: %s and %s" input-1 input-2)]
    (println v)
    v))

#_(defn print-inputs [& inputs]
  (println "The inputs are: " inputs))

#_(defn join-inputs
  [a b]
  (str a "_" b))

;; Subscribe and run
#_(subscribe-and-run [input-channel-1 input-channel-2] process-inputs)



;; Example usage:
;; (go (>!! input-channel-1 "Value 1"))
;; (go (>!! input-channel-2 "Value 2"))


;; if there is no process-fn, we want to always emit the initial output, no need to check anything.
;; just emit the output when an input comes.

;; if there is a process-fn and no initial-output, we want to always run the process-fn when an input arrives.

;; if there is both a process-fn and an initial-output, we always emit the latest good output whenever an input arrives.
;; checking if the inputs have changed, re-running the fn only when the inputs changed from last.

(defn maybe-apply
  [f args]
  (try
    (apply f args)
    (catch Exception e nil)))

#_(defn cell1
  [inputs process-fn initial-output]
  (let [initial-input-vals (vec (repeat (count inputs) nil))
        state              (atom {:previous initial-input-vals
                                  :current  initial-input-vals
                                  :output   initial-output})
        output             (chan (a/sliding-buffer 1))
        output-sub         (create-publisher output)]
    (doseq [[idx input] (map-indexed vector inputs)]
      (let [sub-channel (chan)]
        (sub input :value sub-channel)
        (go-loop []
          (when-let [value (<! sub-channel)]
            ;; store the input's value
            (swap! state assoc-in [:current idx] value)
            ;; this is just for debugging. don't keep it
            (swap! mystate conj @state)
            (let [{:keys [previous current]} @state]
              (cond
                (and process-fn initial-output)
                (when (not= previous current)
                  (swap! state assoc :previous current)
                  (when-let [v (maybe-apply process-fn current)]
                    (swap! state assoc :output v)))

                process-fn
                (when-let [v (maybe-apply process-fn current)]
                  (swap! state assoc :output v))))
            (>! output (or (:output @state) initial-output :no-output))
            (recur)))))
    output-sub))

#_(defonce emitter (cell1 [input-channel-publisher] nil 1))
#_(defonce adder (cell1 [emitter emitter] (fn [a b] (+ a b)) 0))
#_(defonce printer (cell1 [emitter adder] (fn [& args] (println "printing: " args) ) nil))


;; Could we use an event-bus to handle all message passing?
;; instead of focusing on each node's channels directly, they always just pub/sub to the event bus

(defonce event-bus (chan))
(defonce event-publisher (pub event-bus :cell-id))

(defn cell2
  [cell-id inputs process-fn initial-output]
  (let [initial-input-vals (vec (repeat (count inputs) nil))
        state              (atom {:previous initial-input-vals
                                  :current  initial-input-vals
                                  :output   initial-output})]
    (doseq [[idx input-cell-id] (map-indexed vector inputs)]
      (let [sub-channel (chan)]
        (sub event-publisher input-cell-id sub-channel)
        (go-loop []
          (when-let [{:keys [value]} (<! sub-channel)]
            ;; store the input's value
            (swap! state assoc-in [:current idx] value)
            (let [{:keys [previous current]} @state]
              (cond
                (and process-fn initial-output)
                (when (not= previous current)
                  (swap! state assoc :previous current)
                  (when-let [v (maybe-apply process-fn current)]
                    (swap! state assoc :output v)))

                process-fn
                (when-let [v (maybe-apply process-fn current)]
                  (swap! state assoc :output v))))
            (when-let [v (or (:output @state) initial-output)]
              (>! event-bus {:cell-id cell-id
                             :value   v}))
            (recur)))))
    state))


#_(defonce printer2 (cell2 :printer2 [:asdf] (fn [& args] (println "printing: " args) ) nil))


;; I want to have a cell that does not require me to connect any inputs or outputs at creation time.
;; To do that I need some way to store the mapping of inputs to the fn in the cell
;; to which real cell-id the subsriptions should listen for

;;
(defrecord cell-state [cell-id inputs channels process-fn inital-output previous current output control])

(defn- make-cell-state
  [cell-map]
  (map->cell-state cell-map))

(defn- cell3*
  [{:keys [inputs channels initial-output cell-id] :as cell-map}]
  (let [state (atom (make-cell-state cell-map))]
    ;; inputs will be a vector of cell-ids or `nil`. All chans have to be subbed to their given inputs.
    (sub event-publisher cell-id (first channels))
    (doseq [[idx input-id] (map-indexed vector inputs)]
      (let [input-chan (get channels idx)]
        (when input-id ;; cell-id can be nil at first, so the channel has no subs.
          (sub event-publisher input-id input-chan))))

    ;; kick off the go-loop for each input
    (doseq [[idx sub-channel] (map-indexed vector channels)]
      (go-loop []
        (let [{id :cell-id value :value action :action :as wasd} (<! sub-channel)]
          ;; when THIS cell receives an action, perform it
          (when (and action (= id cell-id))
            (case action
              :touch
              (let [{:keys [process-fn current]} @state
                    v                            (maybe-apply process-fn current)]
                (println "TOUCHED: " cell-id "with this message on event-bus: " wasd)
                (when v
                  (swap! state assoc :output v)
                  (>! event-bus {:cell-id cell-id
                                 :value   v})))))
          ;; when this cell receives a value, make sure its not itself!
          (when (and value (not= id cell-id))
            ;; store the input's value
            (swap! state assoc-in [:current idx] value)
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
            (recur)))))
    ;; return the state object so that the cell can be modified
    state))

(defn cell3
  [cell-id inputs process-fn initial-output]
  (let [iinputs            (vec (or inputs [::empty]))
        initial-input-vals (vec (repeat (count inputs) nil))
        input-chans        (vec (repeatedly (count iinputs) #(chan (a/sliding-buffer 1)))) ;; these each need to sub to their appropriate input
        cell-map           {:cell-id       cell-id
                            :inputs        iinputs
                            :channels      input-chans
                            :process-fn    process-fn
                            :inital-output initial-output
                            :previous      initial-input-vals
                            :current       initial-input-vals
                            :output        initial-output
                            :control       :play}]
    (cell3* cell-map)))

(defn- unsub-sub
  [prev current channel]
  (when (and prev current) (a/unsub event-publisher prev channel))
  (when current (a/sub event-publisher current channel)))

(defn connect!
  [new-inputs cell-state]
  (let [{:keys [inputs]} @cell-state
        f                (fn [state]
                           (-> state
                               (assoc :inputs new-inputs)
                               (update :channels (fn [channels]
                                                   (mapv unsub-sub inputs new-inputs channels)))))]
    (swap! cell-state f)))

(defn stop!
  [cell-state]
  (let [{:keys [channels]} @cell-state]
    (doseq [c channels]
      (a/close! c))
    (reset! cell-state nil)))

#_(defonce iinc (cell3 :inc [:number] inc nil))
(defonce printer3 (cell3 :printer3 [:asdf] (fn [& args] (println "printing3: " args) ) nil))

;; How do I make a cell that takes 2 inputs but doesn't start with any of them connected?
;; You can make a cell with a vector of 2 nil inputs:

(defonce adder3 (cell3 :adder [nil nil] + 0))

;; now adder is the atom that holds the current state of that cell.


;; Let's match the existing cell semantics for the spreadsheet. This means I need:
;; - [x] global-cells is an atom of :cell-id to the cell
;; - [x] cell-counter is an increasing cell-id counter
;; - [x] c/formula is a function taking a fn and sources (fn is the process-fn, sources is list of cell-ids)
;; - [x] c/touch! recomputes the given cell
;; - [x] c/destroy! competely removes a cell, including removing it from sources/sinks of other cells
;; - [x] c/value gets the value of the cell
;; - [x] c/reset-function! keeps the cell and the things that depend on it but adds/removes inputs and changes the fn
;; - [x] c/swap-function! swaps only the fn of the cell


;; Let's try instead to build each cell to only use 1 chan. We can sub/unsub that chan to any number of inputs instead.

(defn- input-vec->map
  [inputs]
  (->> inputs
       (map-indexed (fn [idx v] (or v idx)))
       vec
       set/map-invert))

(defn- cell4*
  [{:keys [inputs channel initial-output cell-id] :as cell-map}]
  (let [imap (input-vec->map inputs)
        state (atom (assoc cell-map :imap imap))]
    ;; sub the cell's chan to its own cell-id
    ;; this is to listen for :action messages
    ;; :value messages for the cell-id are filtered out (otherwise it'll trigger an endless loop)
    (sub event-publisher cell-id channel)
    ;; for all inputs, sub to that input-id
    (doseq [id inputs]
      (when id
        (sub event-publisher id channel)))

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

(defn cell4
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
    (cell4* cell-map)))


(comment

  (def a (cell4 {:id 0} nil (fn [] 30) nil))
  (def b (cell4 {:id 1} [{:id 0}] (fn [a] (println "printing: " a)) nil))
  (>!! event-bus {:cell-id {:id 0} :action :touch})
  (def c (cell4 :3print [:a :b :c] (fn [a b c]
                                     (println "I've only printed when new values came in: " a b c)
                                     (+ a b c)) 0))
  (def d (cell4 :1print [:3print] (fn [a] (println "This is the output from 3print: " a)) nil))

  )
