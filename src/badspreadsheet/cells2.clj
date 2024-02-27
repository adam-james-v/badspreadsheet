(ns badspreadsheet.cells2
  (:require
   [clojure.core.async :as a
    :refer [chan go-loop
            pub sub unsub
            >! <! >!! <!!]]
   [clojure.set :as set]
   [clojure.walk :as walk]))

(defn maybe-apply
  [f args]
  (let [args (remove #{::empty} args)]
    (try
      (apply f args)
      (catch Exception _e nil))))

(defonce event-bus (chan #_(a/sliding-buffer 1)))
(defonce event-bus-mult (a/mult event-bus))
(defonce event-publisher (pub event-bus :id))
(defonce action-bus (chan))
(defonce action-publisher (pub action-bus :id))
(defonce cell-counter (atom -1))
(defonce global-cells (atom {:cells {}}))

(defn- new-cell-id
  []
  (swap! cell-counter inc))

(defn- input-vec->map
  [inputs]
  (->> inputs
       (map-indexed (fn [idx v] {(or v idx) [idx]}))
       (apply merge-with concat)))

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
  [{:keys [inputs channel control-channel initial-output id] :as cell-map}]
  (let [imap  (input-vec->map inputs)
        state (atom (assoc cell-map :imap imap))]

    ;; sub the cell's chan to its own id to listen for :action messages
    (sub action-publisher id control-channel)
    (connect-inputs inputs channel)

    (go-loop []
      (if (= :stop (:control @state))
        (do
          (disconnect-inputs inputs channel)
          (a/close! channel)
          (a/close! control-channel)
          (reset! state nil))
        (let [[{input-id :id
                value    :value
                action   :action} ch] (a/alts! [channel control-channel])]
          (if (= ch channel)
            (do
              (let [{:keys [imap]} @state]
                ;; store the input's value
                (doseq [input-idx (get imap input-id)]
                  (swap! state assoc-in [:current input-idx] value)))

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
                    (swap! state assoc :output v :previous current))))

              ;; output a value (cached or computed) onto the event bus
              (when-let [v (or (:output @state) initial-output)]
                (>! event-bus {:id    id
                               :value v})))

            ;; when this cell receives an action, perform it
            (case action
              :stop
              (swap! state assoc :control :stop)

              :touch
              (let [{:keys [process-fn current output]} @state
                    v                                   (maybe-apply process-fn current)]
                (when v
                  (swap! state assoc :output v))
                (>! event-bus {:id    id
                               :value (or v output)}))))
          (recur))))
    ;; return the state object so that the cell can be modified
    state))

(defn cell
  [id inputs process-fn initial-output]
  (let [inputs    (vec inputs)
        init-vals (vec (repeat (count inputs) nil))
        cell-map  {:id              id
                   :inputs          inputs
                   :channel         (chan (a/sliding-buffer 1))
                   :control-channel (chan)
                   :process-fn      process-fn
                   :initial-output  initial-output
                   :previous        init-vals
                   :current         init-vals
                   :output          initial-output}]
    (cell* cell-map)))

(defn touch!
  "Cause the cell to output."
  [id]
  (>!! action-bus {:id id :action :touch}))

(defn destroy!
  "Destroy the cell, making sure to close the chans and stop the go loops."
  [id]
  (let [cell (get-in @global-cells [:cells id])]
    (swap! cell assoc :control :stop)
    (println "destroyed: " id)
    (>!! action-bus {:id id :action :stop})
    (swap! global-cells update-in [:cells] dissoc id)))

(defn reset-cells!
  []
  (reset! cell-counter -1)
  (doseq [id (keys (:cells @global-cells))]
    (destroy! id)))

(defn value
  [id]
  (let [c (get-in @global-cells [:cells id])]
    (:output @c)))

(defn c-get
  [id k]
  (let [c (get-in @global-cells [:cells id])]
    (get @c k)))

(defn c-assoc
  [id k v]
  (let [c (get-in @global-cells [:cells id])]
    (swap! c assoc k v)))

(defn c-merge
  [id m]
  (let [c (get-in @global-cells [:cells id])]
    (swap! c merge m)))

(defn c-dissoc
  [id k]
  (let [c (get-in @global-cells [:cells id])]
    (swap! c dissoc k)))

(defn swap-function!
  [id f]
  (let [cell-state (get-in @global-cells [:cells id])]
    (swap! cell-state assoc :process-fn f)
    (touch! id)
    id))

(defn reset-function!
  [id f inputs]
  (let [cell-state (get-in @global-cells [:cells id])]
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
                 (update :previous (fn [vals] (vec (take (count inputs) vals))))
                 (update :current (fn [vals] (vec (take (count inputs) vals))))
                 (assoc :process-fn f))))
    (touch! id)
    id))

(defn- cell-exists?
  [id]
  (contains? (:cells @global-cells) id))

(defn formula
  "Make a new global cell. Returns the new cell's cellID."
  ([f] (formula (new-cell-id) f []))
  ([f inputs] (formula (new-cell-id) f inputs))
  ([id f inputs]
   (let [c (cell id inputs f (when (seq inputs) ::waiting))]
     (if (cell-exists? id)
       (reset-function! id f inputs)
       (do
         (swap! global-cells assoc-in [:cells id] c)
         (touch! id)
         id)))))

(defn list-cells
  []
  (->> @global-cells
       :cells
       keys
       (filter (complement #{::reader ::linker ::builder}))
       vec))

(comment

  (def acell (formula (fn [] (* 2 3))))
  (def bcell (formula (fn [a] (println "printed: " a)) [acell]))
  (def ccell (formula (fn [a] (+ a 100)) [acell]))
  (def dcell (formula (fn [a] (println "printed: " a)) [ccell]))



  )



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
    (catch Exception e e)))

(defn c# [id] id)

(def ^:private sharp-forms
  {'c# 'badspreadsheet.cells2/c# ;; value by id
   })

(defn collect-sharp-forms
  [form]
  (letfn [(collect [x]
            (cond
              (and (list? x) (seq x) ((set (keys sharp-forms)) (first x))) [x]
              (coll? x) (mapcat collect x)
              :else []))]
    (vec (distinct (collect form)))))

(defn formulize
  [form]
  (let [sharps           (collect-sharp-forms form)
        syms             (mapv (fn [[sym id]] (symbol (format "%s%s" sym id))) sharps)
        smap             (zipmap sharps syms)
        input-cell-forms (vec (walk/postwalk-replace sharp-forms sharps))]
    {:fun         (eval `(fn ~syms
                           (binding [~'*ns* (find-ns '~'user)]
                             ~(walk/postwalk-replace smap form))))
     :form        `(fn ~syms ~(walk/postwalk-replace smap form))
     :input-cells (when (seq input-cell-forms) (map eval input-cell-forms))}))

(defn maybe-formulize
  [form]
  (try
    (formulize form)
    (catch Exception _e
      (println "Error formulizing form."))))

(defn reader*
  [{:keys [s] :as data}]
  (let [v (maybe-read-string s)]
    (if (= (type v) java.lang.RuntimeException)
      (assoc data :error v)
      (assoc data :form v))))

(defn linker*
  [{:keys [form] :as data}]
  (when form
    (merge (dissoc data :error) (formulize form))))

(defn builder*
  [{:keys [id fun input-cells]}]
  (if (cell-exists? id)
    (let [existing-inputs (:inputs @(get-in @global-cells [:cells id]))]
      (if (= existing-inputs input-cells)
        (swap-function! id fun)
        (reset-function! id fun input-cells)))
    (if id
      (formula id fun input-cells)
      (formula fun input-cells))))

#_(defonce reader (formula ::reader #'reader* [::content]))
#_(defonce linker (formula ::linker #'linker* [::reader]))
#_(defonce builder (formula ::builder #'builder* [::linker]))
(defonce b2 (formula ::builder (comp #'builder* #'linker* #'reader*) [::content]))

;; To use the ::builder:
#_
(>!! event-bus {:id ::content
                :s "(* 3 4)"})
