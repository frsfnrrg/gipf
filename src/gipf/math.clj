(ns gipf.core)

(defn sqrt [x] (java.lang.Math/sqrt x))

(defn abs [^long x] (java.lang.Math/abs x))

(defn triangular-number [^long n]
  (/ (* n (inc n)) 2))

(defn hexagonal-number [^long n]
  (+ 1 (* 3 n (dec n))))

(defn reverse-hex-floor
  [^long n]
  (let [d (/ (- (float n) 1) 3)
        v (Math/sqrt d)]
    ;; r-1 < v < r
    (if (> (hexagonal-number (inc (int v))) n)
        (int v)
        (inc (int v)))))

(def sqrt3o3t2 (float (/ (* 2 (java.lang.Math/sqrt 3)) 3)))

(defn round-int
  [^double x]
  (int (java.lang.Math/round x))) 

(defn sign [x]
  (cond
   (pos? x) 1
   (neg? x) -1
   :else 0))
