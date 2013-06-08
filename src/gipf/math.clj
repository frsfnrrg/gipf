(ns gipf.core)

(defn sqrt [x] (java.lang.Math/sqrt x))

(defn abs [x] (java.lang.Math/abs x))

(defn sign [x]
  (cond
   (pos? x) 1
   (neg? x) -1
   :else 0))
