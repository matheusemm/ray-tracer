(ns ray-tracer.float-point)

(defn equal? [^double a ^double b]
  (<= (Math/abs (- a b)) 0.00001))
