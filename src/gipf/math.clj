(ns gipf.core
  (:import (gipfj MathUtil))
  )

(defn sqrt [x] (java.lang.Math/sqrt x))

;; could use defrename
(defn abs [^long x] (java.lang.Math/abs x))

;; MathUtil has a faster version
(defn hexagonal-number [^long n]
  (+ 1 (* 3 n (dec n))))

(def hex4 (long (hexagonal-number 4)))

(def long-zero (long 0))
(def long-4 (long 4))

(defn sign [x]
  (cond
   (pos? x) 1
   (neg? x) -1
   :else 0))


(defrename add `MathUtil/add 2)
(defrename equals `MathUtil/equals 2)
(defrename multiply `MathUtil/multiply 2)
(defrename divide `MathUtil/divide 2)
(defrename subtract `MathUtil/subtract 2)
(defrename inc-1 `MathUtil/inc 1)
(defrename dec-1 `MathUtil/dec 1)
(defrename negate `MathUtil/negate 1)
