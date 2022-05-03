(ns ray-tracer.core
  (:require [ray-tracer.point :as point]
            [ray-tracer.vektor :as vektor]
            [ray-tracer.tuple :as tuple]))

(defn projectile
  [position velocity]
  (assert (point/point? position) "`position` must be a point")
  (assert (vektor/vektor? velocity) "`velocity` must be a vector")
  {:position position, :velocity velocity})

(defn environment
  [gravity wind]
  (assert (vektor/vektor? gravity) "`gravity` must be a vector")
  (assert (vektor/vektor? wind) "`wind` must be a vector")
  {:gravity gravity, :wind wind})

(defn tick
  [{:keys [gravity wind]} {:keys [position velocity]}]
  (projectile (tuple/add position velocity)
              (tuple/add velocity gravity wind)))

(defn -main [& args]
  (loop [e (environment (vektor/vektor 0 -0.1 0)
                        (vektor/vektor -0.01 0 0))
         p (projectile (point/point 0 1 0)
                       (tuple/normalize (vektor/vektor 1 1 0)))]
    (println (:position p))
    (let [next-p (tick e p)]
      (if (> (-> next-p :position tuple/y) 0)
        (recur e next-p)))))
