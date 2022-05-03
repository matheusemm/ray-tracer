(ns ray-tracer.vektor
  (:require [ray-tracer.float-point :as fp]
            [ray-tracer.tuple :as tuple]))

(defn vektor
  [x y z]
  (tuple/tuple x y z 0.0))

(defn vektor?
  [v]
  (assert (= (count v) 4))
  (fp/equal? (nth v 3) 0.0))

(defn dot
  [v1 v2]
  (assert (vektor? v1) "`v1` must be a vector")
  (assert (vektor? v2) "`v2` must be a vector")
  (reduce + (mapv * v1 v2)))

(defn cross
  [[x1 y1 z1 :as v1] [x2 y2 z2 :as v2]]
  (assert (vektor? v1) "`v1` must be a vector")
  (assert (vektor? v2) "`v2` must be a vector")
  (vektor
    (- (* y1 z2) (* z1 y2))
    (- (* z1 x2) (* x1 z2))
    (- (* x1 y2) (* y1 x2))))
