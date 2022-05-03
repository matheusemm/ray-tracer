(ns ray-tracer.tuple-test
  (:require [ray-tracer.float-point :as fp]
            [ray-tracer.point :as point]
            [ray-tracer.tuple :as tuple]
            [ray-tracer.vektor :as vektor]
            [clojure.test :refer :all]))

(deftest factory-fns
  (testing "A tuple with w=1.0 is a point"
    (let [a (tuple/tuple 4.3 -4.2 3.1 1.0)]
      (is (fp/equal? (tuple/x a) 4.3))
      (is (fp/equal? (tuple/y a) -4.2))
      (is (fp/equal? (tuple/z a) 3.1))
      (is (fp/equal? (tuple/w a) 1.0))
      (is (point/point? a) (str a " should be a point"))
      (is (not (vektor/vektor? a)) (str a " shouldn't be a vector"))))

  (testing "A tuple with w=0.0 is a vector"
    (let [a (tuple/tuple 4.3 -4.2 3.1 0.0)]
      (is (fp/equal? (tuple/x a) 4.3))
      (is (fp/equal? (tuple/y a) -4.2))
      (is (fp/equal? (tuple/z a) 3.1))
      (is (fp/equal? (tuple/w a) 0.0))
      (is (not (point/point? a)) (str a " shouldn't be a point"))
      (is (vektor/vektor? a) (str a " should be a vector"))))

  (testing "(point) creates a tuple with w=1"
    (let [p (point/point 4 -4 3)]
      (is (tuple/equal? p (tuple/tuple 4 -4 3 1)))))

  (testing "(vektor) creates a tuple with w=0"
    (let [v (vektor/vektor 4 -4 3)]
      (is (tuple/equal? v (tuple/tuple 4 -4 3 0))))))

(deftest operations
  (testing "Adding two tuples"
    (let [a1 (tuple/tuple 3 -2 5 1)
          a2 (tuple/tuple -2 3 1 0)]
      (is (tuple/equal? (tuple/add a1 a2)
                        (tuple/tuple 1 1 6 1)))))

  (testing "Subtracting two points"
    (let [p1 (point/point 3 2 1)
          p2 (point/point 5 6 7)]
      (is (tuple/equal? (tuple/sub p1 p2)
                        (vektor/vektor -2 -4 -6)))))

  (testing "Subtracting a vector from a point"
    (let [p (point/point 3 2 1)
          v (vektor/vektor 5 6 7)]
      (is (tuple/equal? (tuple/sub p v)
                        (point/point -2 -4 -6)))))

  (testing "Subtracting two vectors"
    (let [v1 (vektor/vektor 3 2 1)
          v2 (vektor/vektor 5 6 7)]
      (is (tuple/equal? (tuple/sub v1 v2)
                        (vektor/vektor -2 -4 -6)))))

  (testing "Subtracting a vector from the zero vector"
    (let [zero (vektor/vektor 0 0 0)
          v (vektor/vektor 1 -2 3)]
      (is (tuple/equal? (tuple/sub zero v)
                        (vektor/vektor -1 2 -3)))))

  (testing "Negating a tuple"
    (let [a (tuple/tuple 1 -2 3 -4)]
      (is (tuple/equal? (tuple/neg a)
                        (tuple/tuple -1 2 -3 4)))))

  (testing "Multiplying a tuple by a scalar"
    (let [a (tuple/tuple 1 -2 3 -4)]
      (is (tuple/equal? (tuple/mul a 3.5)
                        (tuple/tuple 3.5 -7 10.5 -14)))))

  (testing "Multiplying a tuple by a fraction"
    (let [a (tuple/tuple 1 -2 3 -4)]
      (is (tuple/equal? (tuple/mul a 0.5)
                        (tuple/tuple 0.5 -1 1.5 -2)))))

  (testing "Dividing a tuple by a scalar"
    (let [a (tuple/tuple 1 -2 3 -4)]
      (is (tuple/equal? (tuple/div a 2)
                        (tuple/tuple 0.5 -1 1.5 -2)))))

  (testing "Computing the magnitude of unit vectors"
    (doseq [t [(tuple/tuple 1 0 0)
               (tuple/tuple 0 1 0)
               (tuple/tuple 0 0 1)]]
      (is (fp/equal? (tuple/magnitude t) 1.0))))

  (testing "Computing the magnitude of non-unit vectors"
    (doseq [t [(tuple/tuple 1 2 3)
               (tuple/tuple -1 -2 -3)]]
      (is (fp/equal? (tuple/magnitude t)
                     (Math/sqrt 14)))))

  (testing "Normalizing vector (4, 0, 0) gives (1, 0, 0)"
    (let [v (vektor/vektor 4 0 0)]
      (is (tuple/equal? (tuple/normalize v)
                        (vektor/vektor 1 0 0)))))

  (testing "Normalizing vector (1 2 3)"
    (let [v (vektor/vektor 1 2 3)]
      (is (tuple/equal? (tuple/normalize v)
                        (vektor/vektor 0.26726 0.53452 0.80178)))))

  (testing "The magnitude of a normalized vector"
    (let [v (vektor/vektor 1 2 3)
          norm (tuple/normalize v)]
      (is (fp/equal? (tuple/magnitude norm) 1.0)))))
