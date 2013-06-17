(ns gipf.core)

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
