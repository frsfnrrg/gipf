(ns gipf.core
  (:import (gipfj MathUtil))
  )

(defn sqrt [x] (java.lang.Math/sqrt x))

;; could use definline
(defn abs [^long x] (java.lang.Math/abs x))

;; MathUtil has a faster version
(defn hexagonal-number [^long n]
  (+ 1 (* 3 n (dec n))))

(def long-zero (long 0))
(def long-4 (long 4))

(defn sign [x]
  (cond
   (pos? x) 1
   (neg? x) -1
   :else 0))


(definline add [a b]`( MathUtil/ladd ~a ~b))
(definline equals [a b]`( MathUtil/lequals ~a ~b))
(definline multiply [a b]`( MathUtil/lmultiply ~a ~b))
(definline divide [a b]`( MathUtil/ldivide ~a ~b))
(definline subtract [a b]`( MathUtil/lsubtract ~a ~b))
(definline fastmax [a b]`( MathUtil/lmax ~a ~b))
(definline fastmin [a b]`( MathUtil/lmin ~a ~b))
(definline greater [a b] `( MathUtil/lgreater ~a ~b))
(definline less [a b] `( MathUtil/lless ~a ~b))

(definline inc-1 [a] `(MathUtil/linc ~a) )
(definline dec-1 [a] `(MathUtil/ldec ~a))
(definline negate [a] `( MathUtil/lnegate ~a))
(definline fast-even? [a] `( MathUtil/levenp ~a))
(definline fast-odd? [a] `( MathUtil/loddp ~a))

(definline greater-equals [a b]
  `(MathUtil/lgreaterEquals ~a ~b))
(definline less-equals [a b]
  `(MathUtil/llesserEquals ~a ~b))


(def positive-infinity Ranking/POS_INF)
(def negative-infinity Ranking/NEG_INF)

;; the issue with max/min value is
;; that they make rollover
;; exception waaay to common. Why think
;; about it when you don't have to?

;;java.lang.Long/MAX_VALUE
;;java.lang.Long/MIN_VALUE
