(ns ray-tracer.point
  (:require [ray-tracer.float-point :as fp]
            [ray-tracer.tuple :as tuple]))

(defn point
  [x y z]
  (tuple/tuple x y z 1.0))

(defn point?
  [p]
  (assert (= (count p) 4))
  (fp/equal? (nth p 3) 1.0))
