(ns gipf.core)
;; tools for hex geometry

;; see ?redblobgames; hex coordinate grids.
;; the best normalize is (= 0 (+ u v w)). distance is (reduce sum map abs map -)

(defrecord UV [^int u ^int v])
(defrecord XY [x y])


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
  (UV. (- u w) (+ v w)))

(def xy-u (xy 0 -1))
(def xy-v (xy (/ (Math/sqrt 3) 2) -0.5))
(def xy-w (xy (/ (Math/sqrt 3) 2) 0.5))

(defn pt=
  [& pts]
  (apply = pts))

(defn getU [pt] (:u pt))
(defn getV [pt] (:v pt))

(defn pt+
  ([] (UV. 0 0))
  ([pt] pt)
  ([a b] (UV. (+ (:u a) (:u b)) (+ (:v a) (:v b))))
  ([a b & pts]
      (UV. (apply + (:u a) (:u b) (map getU pts))
           (apply + (:v a) (:v b) (map getV pts)))))

(defn pt-
  ([] (UV. 0 0))
  ([p] (UV. (- (:u p)) (- (:v p))))
  ([p q] (UV. (- (:u p) (:u q)) (- (:v p) (:v q))))
  ([p q & rest] (pt- p (apply pt+ q rest))))

(defn pt* [factor p]
  (UV. (* factor (:u p)) (* factor (:v p))))

(defn xy+ [& xys]
  (apply map + xys))
(defn xy- [& xys]
  (apply map - xys))
(defn xy* [factor xy]
  (map * (repeat factor) xy))

(defn pt->xy
  "Converts a pt-point to a xy-point."
  [pt]
  (xy+ (xy* (:u pt) xy-u) (xy* (:v pt) xy-v)))

;; converts to a (u v 0). We coerce coords to int
(defn xy->pt-int
  "Converts an xy-point to a pt-point."
  [z]
  (pt (round-int (- (/ (first z) 2) (second z))) 0 (round-int (* (first z) sqrt3o3t2))))

(defn interpolate-list
  [point-a point-b count]
  (let [delta (pt* (/ count) (pt- point-b point-a))]
    (loop [made (list) c 0]
      (if (= c count)
        made
        (recur (cons (pt+ point-a (pt* c delta)) made) (inc c))))))

(def pt-origin (UV. 0 0))

(def pt-radius
  (memoize
   (fn [p]
     (if (pt= p pt-origin)
       0
       (let [u (:u p)
             v (:v p)]
         (cond (= u v 0) 0
               (and (= v 0) (> u 0)) u
               (and (= u (- v)) (> u 0)) u
               (and (= u 0) (< v 0))  (- v)
               (and (= v 0) (< u 0))  (- u)
               (and (= (- u) v) (< u 0)) v
               (and (= u 0) (> v 0)) v
               
               (> u (- v) 0)          u
               (> (- v) u 0)       (- v)
               (and (< u 0) (< v 0))      (- (+ u v))
               (> (- u) v 0)      (- u)
               (> v (- u) 0)          v
               (and (> u 0) (> v 0))   (+ u v)))))))

(defn pt-dist
  [pta ptb]
    (pt-radius (pt- pta ptb)))

(defn pt-rot+60
  [p]
  (pt (:v p) 0 (- (:u p))))

(defn pt-rot-60
  [p]
  (pt 0 (:u p) (:v p)))
  
  
  
  
  
  
  
  
  


