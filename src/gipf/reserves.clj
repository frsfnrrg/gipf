(println "HEY")

(ns gipf.core)

;; if we need still more optimization, store as a single number.
;; use +- 64; bitshift, c++; fortran, assembly, asics...
(defrecord Reserves [^long p1 ^long p2])

(defn inc-reserves
  [ ^Reserves r ^long player]
  (if (> player (long long-zero))
    (->Reserves (unchecked-inc (long (:p1 r))) (:p2 r))
    (->Reserves (:p1 r) (unchecked-inc (long (:p2 r))))))

(defn dec-reserves
  [^Reserves r ^long player]
  (if (> player long-zero)
    (->Reserves (unchecked-dec (long (:p1 r))) (:p2 r))
    (->Reserves (:p1 r) (unchecked-dec (long (:p2 r))))))

(defn get-reserves
  [^Reserves r ^long player]
  (if (> player long-zero)
    (:p1 r)
    (:p2 r)))
