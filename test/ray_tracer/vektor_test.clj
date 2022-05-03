(ns ray-tracer.vektor-test
  (:require [ray-tracer.float-point :as fp]
            [ray-tracer.tuple :as tuple]
            [ray-tracer.vektor :as vektor]
            [clojure.test :refer :all]))

(deftest operations
  (testing "The dot product of two vectors"
    (let [a (vektor/vektor 1 2 3)
          b (vektor/vektor 2 3 4)]
      (fp/equal? (vektor/dot a b) 20)))

  (testing "The cross product of two vectors"
    (let [a (vektor/vektor 1 2 3)
          b (vektor/vektor 2 3 4)]
      (tuple/equal? (vektor/cross a b)
                    (vektor/vektor -1 2 -1))
      (tuple/equal? (vektor/cross b a)
                    (vektor/vektor 1 -2 1)))))
