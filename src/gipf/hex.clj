(ns gipf.core)

(defn make-hex-array 
  []
  ;; has an immutable radius, and internal fields.
  
  ;; internal structure: a vector containing the fields, arranged in ccw order..
  ;; to access, we have a 1:1 mapping function to the fields
  
  ;;       7
  ;;     8   I
  ;;   9   1   H
  ;;     2   6
  ;;   A   0   G
  ;;     3   5  
  ;;   B   4   F
  ;;     C   E
  ;;       D
  (fill-vector (constantly 0) (hexagonal-number 4)))


(def unit-ring-points-cycled
  (list (pt 1 0 0) (pt 0 1 0) (pt 0 0 1) (pt -1 0 0) (pt 0 -1 0) (pt 0 0 -1) (pt 1 0 0)))

(def unit-ring-points
  (list (pt 1 0 0)
        (pt 0 1 0)
        (pt 0 0 1)
        (pt -1 0 0)
        (pt 0 -1 0)
        (pt 0 0 -1)))

(defn
  get-ring-of-hex-uv-points
  [^long radius]
  
  (if (= 0 radius)
    (list (pt 0 0 0))
  
    (loop [made (list) pointsleft unit-ring-points-cycled]
      (if (empty? (rest pointsleft))
        made
        (recur (concat made
                       (reverse (interpolate-list
                                 (pt* radius (first pointsleft))
                                 (pt* radius (second pointsleft))
                                 radius)) )
               (rest pointsleft))))))

(def get-hex-array get)

(def arrayfull-of-points (vec (range (hexagonal-number 4))))

(def change-hex-array assoc)

(defn map-hex-array
  [func-of-pt & arrays]
  (apply mapv func-of-pt arrayfull-of-points arrays))

(defn count-over-hex-array
  "Takes a boolean function of a position and
  values, and returns the number of
  times it is true over the arrays."
  [func-of-pt & arrays]
  (reduce +
          (apply map-hex-array
                 (fn [& args] (if (apply func-of-pt args) 1 0))
                 arrays)))

