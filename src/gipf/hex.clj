(ns gipf.core)

;; the reverse op is simple, but never needed
(defn pt->n
  "Maps a pt onto the whole numbers. See make-hex-array."
  [p]
  ;           u
  ;       +-0-+
  ;       |._ |.5
  ;       1  .|  .
  ;       +---+---+ v
  ;        ._ |._ 4
  ;         2.|  .|
  ;           +-3-+
  ;
  
  (let [value (fn [layer segment prog]
                (int (+ (hexagonal-number layer) (* layer segment) prog)))
        u (int (:u p))
        v (int (:v p))]
    (cond (= u v 0)
          (int 0)
          ;; axial points - merge into segments?
          (and (= v 0) (> u 0))
          (value u 0 0)
          (and (= u (- v)) (> u 0))
          (value u 1 0)
          (and (= u 0) (< v 0))
          (value (- v) 2 0)
          (and (= v 0) (< u 0))
          (value (- u) 3 0)
          (and (= (- u) v) (< u 0))
          (value v 4 0)
          (and (= u 0) (> v 0))
          (value v 5 0)
          
          ;; segments!!
          (> u (- v) 0) ;; segment 0
          (value u 0 (- v))
          (> (- v) u 0) ;; segment 1
          (value (- v) 1 (- (- v) u))
          (and (< u 0) (< v 0)) ;; segment 2
          (value (- (+ u v)) 2 (- u))
          (> (- u) v 0) ;; segment 3
          (value (- u) 3 v)
          (> v (- u) 0) ;; segment 4
          (value v 4 (- v (- u)))
          (and (> u 0) (> v 0)) ;; segment 5
          (value (+ u v) 5 u))))

(def n->pt
  (memoize
   (fn
     [^long n]
     (if (= n 0)
       (pt 0 0 0)
       (let [layer (reverse-hex-floor n)
             nring (- n (hexagonal-number layer))
             prg (mod nring layer)
             segment (/ (- nring prg) layer)]
         (case segment
           0 (pt layer (- prg) 0)
           1 (pt (- layer prg) (- layer) 0)
           2 (pt (- prg) (- prg layer) 0)
           3 (pt (- layer) prg 0)
           4 (pt (- prg layer) layer 0)
           5 (pt prg (- layer prg) 0)))))))

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


(defn
  get-ring-of-hex-uv-points
  [radius]

  (if (= 0 radius)
    (list (pt 0 0 0))
  
    (let [ringpoints (list (pt 1 0 0) (pt 0 1 0) (pt 0 0 1) (pt -1 0 0) (pt 0 -1 0) (pt 0 0 -1) (pt 1 0 0))]
      (loop [made (list) pointsleft ringpoints]
        (if (empty? (rest pointsleft))
          made
          (recur (concat made
                   (reverse (interpolate-list
                              (pt* radius (first pointsleft))
                              (pt* radius (second pointsleft))
                              radius)))
            (rest pointsleft)))))))


(defn get-hex-array
  [array pt]
  (get array (pt->n pt)))

;; how should one normalize pts?

(def arrayfull-of-points (vec (map n->pt (range (hexagonal-number 4)))))

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
