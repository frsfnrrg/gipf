(ns gipf.core)
;; tools for hex geometry

;; see ?redblobgames; hex coordinate grids.
;; the best normalize is (= 0 (+ u v w)). distance is (reduce sum map abs map -)

(defn xy
  [x y]
  "Create a cartesian coordinate pont.

    y
    |
    |
    X--x
"
  (list x y))

;; or use deftype? since pt= != plain =, and we want those to be true..
(defn pt
  "Create a hexagonal coordinate point.


      u
      |  v
      | / 
      |/
      X
       \\
        \\
         w
"
  [u v w]
  (list u v w))

(def xy-u (xy 0 -1))
(def xy-v (xy (/ (Math/sqrt 3) 2) -0.5))
(def xy-w (xy (/ (Math/sqrt 3) 2) 0.5))

(defn pt=
  [& pts]
  (pairwise-and (fn [a b]
                  (and 
                    (= (+ (first a) (/ (second a) 2) (/ (third a) -2))
                         (+ (first b) (/ (second b) 2) (/ (third b) -2))) ; vert
                    (= (+ (second a) (third a))
                      (+ (second b) (third b))))) ; horizontal
    pts))

(defn pt+ [& pts]
  (apply map + pts))
(defn pt- [& pts]
  (apply map - pts))
(defn pt* [factor pt]
  (map * (repeat factor) pt))

(defn xy+ [& xys]
  (apply map + xys))
(defn xy- [& xys]
  (apply map - xys))
(defn xy* [factor xy]
  (map * (repeat factor) xy))

(defn pt->xy
  "Converts a pt-point to a xy-point."
  [pt]
  (xy+ (xy* (first pt) xy-u) (xy* (second pt) xy-v) (xy* (third pt) xy-w)))

(def sqrt3o3t2 (/ (* 2 (java.lang.Math/sqrt 3)) 3))

;; converts to a (u v 0). We coerce coords to int
(defn xy->pt
  "Converts an xy-point to a pt-point."
  [z]
  (pt (float (- (/ (first z) 2) (second z))) 0 (float (* (first z) sqrt3o3t2))))

(defn round-int
  [x]
  (int ^int (java.lang.Math/round ^float x))) 

(defn pt-int
  "Coerces coords of a pt to int."
  [p]
  (apply pt (map round-int p)))

(defn interpolate-list
  [point-a point-b count]
  (let [delta (pt* (/ count) (pt- point-b point-a))]
    (loop [made (list) c 0]
      (if (= c count)
        made
        (recur (cons (pt+ point-a (pt* c delta)) made) (inc c))))))

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

(defn triangular-number [n]
  (/ (* n (inc n)) 2))

(defn hexagonal-number [n]
  (+ 1 (* 3 n (dec n))))

(defn make-hex-array 
  [thunk radius]
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
  (list
    radius
    (fill-vector thunk (hexagonal-number radius))))

(defn normalize-to-uv [p]
  (pt (- (first p) (third p)) (+ (second p) (third p)) 0)) 

(defn reverse-hex-floor
  [n]
  (let [d (/ (- n 1) 3)
        v (Math/sqrt d)]
    ;; r-1 < v < r
    (if (> (hexagonal-number (inc (int v))) n)
        (int v)
        (inc (int v)))))

;; the reverse op is simple, but never needed
(defn pt->n
  "Maps a pt onto the whole numbers. See make-hex-array."
  [pt]
  ;           u
  ;       +-0-+
  ;       |._ |.5
  ;       1  .|  .
  ;       +---+---+ v
  ;        ._ |._ 4
  ;         2.|  .|
  ;           +-3-+
  ;
  
  ;; is there no simple closed form??
  ;; what is the best normalization? like the hex-ring-production?

  (defn value [layer segment prog]
    (+ (hexagonal-number layer) (* layer segment) prog))
  
  (let [[u v] (normalize-to-uv (pt-int pt))]
    (cond 
          ;; origin
          (= u v 0)
          0
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

(defn pt-radius
  [p]
  (reverse-hex-floor (pt->n p)))

(defn n->pt
  [n]
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
          5 (pt prg (- layer prg) 0)))))


(defn get-hex-array
  [array pt]
  (get (second array) (pt->n pt)))

;; how should one normalize pts?

(defn map-hex-array
  [func-of-pt & arrays]
  (list 
    (apply min (map first arrays))
    (apply mapvc (fn [c & cells] (apply func-of-pt (n->pt c) cells)) (map second arrays))))

(defn count-over-hex-array
  "Takes a boolean function of a position and values, and returns the number of
times it is true over the arrays."
  [func-of-pt & arrays]
  (reduce + (second (apply map-hex-array
                           (fn [& args] (if (apply func-of-pt args) 1 0))
                           arrays))))

(defn pt-dist
  [pta ptb]
    (pt-radius (pt- pta ptb)))

(defn pt-rot+60
  [p]
  (pt (second p) (third p) (- (first p))))

(defn pt-rot-60
  [p]
  (pt (- (third p)) (first p) (second p)))

   
  
  
  
  
  
  
  
  
  
  


