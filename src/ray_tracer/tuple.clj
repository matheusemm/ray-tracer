(ns ray-tracer.tuple
  (:require [ray-tracer.float-point :as fp]))

(defn tuple
  [x & xs]
  (into [(double x)] (map double xs)))

(defn x
  [t]
  (assert (> (count t) 0))
  (nth t 0))

(defn y
  [t]
  (assert (> (count t) 1))
  (nth t 1))

(defn z
  [t]
  (assert (> (count t) 2))
  (nth t 2))

(defn w
  [t]
  (assert (> (count t) 3))
  (nth t 3))

(defn equal?
  ([_] true)
  ([t1 t2]
   (every? (partial apply fp/equal?)
           (mapv vector t1 t2)))
  ([t1 t2 & ts]
   (let [eq? (equal? t1 t2)]
     (if (and eq? (seq ts))
       (apply equal? t2 (first ts) (next ts))
       eq?))))

(defn add
  ([t] t)
  ([t1 t2]
   (mapv + t1 t2))
  ([t1 t2 & ts]
   (reduce add (add t1 t2) ts)))

(defn sub
  ([t] t)
  ([t1 t2]
   (mapv - t1 t2))
  ([t1 t2 & ts]
   (reduce sub (sub t1 t2) ts)))

(defn neg
  [t]
  (mapv - t))

(defn mul
  [t scalar]
  (mapv #(* % (double scalar)) t))

(defn div
  [t scalar]
  (mapv #(/ % (double scalar)) t))

(defn magnitude
  [t]
  (Math/sqrt (transduce (map #(Math/pow % 2)) + t)))

(defn normalize
  [t]
  (div t (magnitude t)))
