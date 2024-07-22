(ns badspreadsheet.cell-test
  (:require
   [badspreadsheet.cell :as cell]
   [clojure.test :as t :refer [deftest testing is]]))

(defn equivalent-forms? [expected actual]
  (let [symbol-map (atom {})]
    (letfn [(compare-symbols [exp-sym act-sym]
              (if-let [mapped-sym (get @symbol-map exp-sym)]
                (= mapped-sym act-sym)
                (do
                  (swap! symbol-map assoc exp-sym act-sym)
                  true)))

            (compare-forms [exp act]
              (cond
                (and (symbol? exp) (symbol? act))
                (compare-symbols exp act)

                (and (seq? exp) (seq? act))
                (and (= (count exp) (count act))
                     (every? true? (map compare-forms exp act)))

                (and (vector? exp) (vector? act))
                (and (= (count exp) (count act))
                     (every? true? (map compare-forms exp act)))

                (and (map? exp) (map? act))
                (and (= (count exp) (count act))
                     (every? true? (map compare-forms (keys exp) (keys act)))
                     (every? true? (map compare-forms (vals exp) (vals act))))

                :else (= exp act)))]

      (compare-forms expected actual))))

(defn g= [actual expected]
  (equivalent-forms? expected actual))

(deftest process-form-test
  (testing "forms with no refs"
    (doseq [{:keys [input output]}
            [{:input  '(do 2)
              :output '(clojure.core/fn [] 2)}

             {:input  '(do 2 3)
              :output '(clojure.core/fn [] 2 3)}

             {:input  '(do [1 2 3 4])
              :output '(clojure.core/fn [] [1 2 3 4])}

             {:input  '(do (* 2 3))
              :output '(clojure.core/fn [] (* 2 3))}

             {:input  '(do (* 2 3) (* 2 3))
              :output '(clojure.core/fn [] (* 2 3) (* 2 3))}

             {:input  '(do (def a 2) a)
              :output '(clojure.core/fn [] (def a 2) a)}]]
      (is (= output
             (:processed-form (cell/process-form [0 0] input))))))

  (testing "forms with refs"
    (doseq [{:keys [input output]}
            [{:input  '(do (c# :id 0))
              :output '(clojure.core/fn [refa] (clojure.core/let [c#a refa] c#a))}

             {:input  '(do (+ (c# :id 0) (c# :id 1)))
                :output '(clojure.core/fn [refa refb]
                           (clojure.core/let [c#a refa
                                              c#b refb]
                             (+ c#a c#b)))}

             {:input '(do [(c# :id 0) (c# :id 1) (c# :id 1) (c# :id 2)])
              :output '(clojure.core/fn [refa refb refc]
                         (clojure.core/let [c#a refa
                                            c#b refb
                                            c#c refc]
                           [c#a c#b c#b c#c]))}]]
      (is (g= output
              (:processed-form (cell/process-form [0 0] input)))))))
