(ns badspreadsheet.util)

(defn window
  ([[x y] w h] (window [x y] [(+ x w) (+ y h)]))
  ([[x1 y1] [x2 y2]]
   (let [[x1 x2] (sort [x1 x2])
         [y1 y2] (sort [y1 y2])]
     (for [x (range x1 (inc x2))
           y (range y1 (inc y2))]
       [x y]))))
