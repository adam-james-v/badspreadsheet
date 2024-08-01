(ns badspreadsheet.util
  (:require
   [clojure.walk :as walk]))

(defn window
  ([[x y] w h] (window [x y] [(+ x w) (+ y h)]))
  ([[x1 y1] [x2 y2]]
   (let [[x1 x2] (sort [x1 x2])
         [y1 y2] (sort [y1 y2])]
     (for [x (range x1 x2 #_(inc x2))
           y (range y1 y2 #_(inc y2))]
       [x y]))))

(defn resolve-symbol [sym ns]
  (if (symbol? sym)
    (or (get (ns-aliases ns) sym)
        (ns-resolve ns sym)
        sym)
    sym))

(defn fully-resolve-form [form ns-sym]
  (let [ns (find-ns ns-sym)]
    (letfn [(resolve-walker [x]
              (cond
                (and (seq? x) (contains? #{'require 'ns} (first x)))
                x  ; Don't resolve inside require or ns forms

                (symbol? x)
                (let [resolved (resolve-symbol x ns)]
                  (cond
                    (class? resolved) (symbol (.getName ^Class resolved))
                    (var? resolved) (symbol (str (.name (.ns ^clojure.lang.Var resolved)))
                                            (str (.sym ^clojure.lang.Var resolved)))
                    :else resolved))

                (seq? x)
                (walk/walk resolve-walker identity x)

                :else x))]
      (resolve-walker form))))
