(ns gipf.core)
;; tools for hex geometry

;; see ?redblobgames; hex coordinate grids.
;; the best normalize is (= 0 (+ u v w)). distance is (reduce sum map abs map -)

(defrecord UV [^int u ^int v])
(defrecord XY [x y])

;; TODO: remember, it is best to have overkill caching...

;; hey! there is another optimization doable...
;; Let a point be represented by its (pt->n); then, for
;; negation, multiplication, division, addition
;; use lookup-tables

(defn xy
  [x y]
  "Create a cartesian coordinate pont.

    y
    |
    |
    X--x
"
  (list x y))


(defn pt-lookup-table-optimize
  "Optimizing macro. Read the source."
  [srd func]
  (let [qrd (int srd)
         qrn (int (- srd))
         qrl (int (+ 1 (* 2 srd)))
         qra (int (* (inc qrl) qrd))
         table (make-array java.lang.Long/TYPE (* qrl qrl))]

    (doseq [u (range qrn (inc qrd) 1)
            v (range qrn (inc qrd) 1)]
      (aset-long table
                 (+ (+ v qra) 
                    (* u qrl))
                 (func (UV. u v))))
     
     (fn [^UV p]
       (aget ^longs table ;(.clojure.lang.Numbers longs table#)
             (unchecked-add
              (unchecked-add (:v p) qra)
              (unchecked-multiply (:u p) qrl))))))

;; the reverse op is simple, but never needed
(def uv->n
  
  "Maps a pt onto the whole numbers. See make-hex-array."
  (fn
    [^UV p]
    ;;           u
    ;;       +-0-+
    ;;       |._ |.5
    ;;       1  .|  .
    ;;       +---+---+ v
    ;;        ._ |._ 4
    ;;         2.|  .|
    ;;           +-3-+
    ;;
    
    (let [value (fn [^long layer ^long segment ^long prog]
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
            (value (+ u v) 5 u)))))


(def n->uv
  "Rarely called"
  (memoize
   (fn
     [^long n]
     (if (= n 0)
       (UV. 0 0)
       (let [layer (reverse-hex-floor n)
             nring (- n (hexagonal-number layer))
             prg (mod nring layer)
             segment (int (/ (- nring prg) layer))]
         (case segment
           0 (UV. layer (- prg))
           1 (UV. (- layer prg) (- layer))
           2 (UV. (- prg) (- prg layer))
           3 (UV. (- layer) prg)
           4 (UV. (- prg layer) layer)
           5 (UV. prg (- layer prg))))))))

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
  [^long u ^long v ^long w]
  (uv->n (UV. (- u w) (+ v w))))


(defn optimize1-uv-n-pt
  [^long size func]
  (let [table (make-array java.lang.Long/TYPE size)]
    (doseq [n (range size)]
      (aset-long table
            n
            (uv->n (func (n->uv n)))))
     
     (fn [^long p]
       (aget ^longs table p))) )

(defn optimize2-uv-n-pt
  "Optimizes a  UV x UV -> UV to be a N X N -> N"
  [^long size func]

  (time (let [table (make-array java.lang.Long/TYPE size size)]
          (doseq [v (range size)                                   
                  u (range size)]
            (aset-long table
                       u v
                       (uv->n (func (n->uv u) (n->uv v)))))
          (fn [^long u ^long v]                                     
            (deep-aget longs table
                  u v )))))


(defn optimize-s12-uv-n-pt
  "Isize is a radius; nsize is absolute"
  [^long isize ^long nsize func]

  (let [ispan (long (inc (* 2 isize)))
        iadd (long isize)
        table (make-array java.lang.Long/TYPE (* ispan nsize))]
    (doseq [u (range (- isize) (inc isize) 1)
            v (range nsize)]
      (aset-long table
                 (+ (+ u iadd) (* ispan v))
                 (uv->n (func u (n->uv v)))))
    
    (fn [^long p ^long q]
      (aget ^longs table (unchecked-add (unchecked-add p iadd)
                                        (unchecked-multiply q ispan))))))

(def xy-u (xy 0 -1))
(def xy-v (xy (/ (Math/sqrt 3) 2) -0.5))
(def xy-w (xy (/ (Math/sqrt 3) 2) 0.5))

(defn uv=
  "Are two points equal?"
  [^UV pta ^UV ptb]
  (= pta ptb))

(defn pt=
  [^long pta ^long ptb]
  (= pta ptb))

(defn getU [pt] (:u pt))
(defn getV [pt] (:v pt))

(defn uv+
  ([] (UV. 0 0))
  ([^UV pt] pt)
  ([^UV a ^UV b] (UV. (unchecked-add (:u a) (:u b))
              (unchecked-add (:v a) (:v b))))
  ([^UV a ^UV b & pts]
     ;; need some optimized boolean reduce.... Check core.reducers.
      (UV. (apply + (:u a) (:u b) (map getU pts))
           (apply + (:v a) (:v b) (map getV pts)))))

(def pt+
  (optimize2-uv-n-pt
      (hexagonal-number 5)
      uv+))

(defn uv-
  ([] (UV. 0 0))
  ([^UV p] (UV. (unchecked-negate (:u p)) (unchecked-negate (:v p))))
  ([^UV p ^UV q] (UV. (unchecked-subtract (:u p) (:u q))
              (unchecked-subtract (:v p) (:v q))))
  ([^UV p ^UV q & rest] (uv- p (apply uv+ q rest))))

(def pt-neg
  (optimize1-uv-n-pt
                (hexagonal-number 9)
                uv-))

(def pt-sub
  (optimize2-uv-n-pt
                (hexagonal-number 6)
                uv-))


(defn pt-
  ([^long p] (pt-neg p))
  ([^long p ^long q] (pt-sub p q)))

(defn uv* [factor ^UV p]
  (UV. (* factor (:u p)) (* factor (:v p))))

(def pt*
  (optimize-s12-uv-n-pt
   7
   (hexagonal-number 5)
   uv*))

(defn uv-div [factor ^UV p]
  (if (== factor 0) (UV. 0 0)
      (UV. (round-int (/ (:u p) factor)) (round-int (/ (:v p) factor)))))

(def pt-div
  (optimize-s12-uv-n-pt
   7
   (hexagonal-number 5)
   uv-div))

(defn xy+ [& xys]
  (apply map + xys))
(defn xy- [& xys]
  (apply map - xys))
(defn xy* [factor xy]
  (map * (repeat factor) xy))

(defn uv->xy
  "Converts a pt-point to a xy-point."
  [^UV pt]
  (xy+ (xy* (:u pt) xy-u) (xy* (:v pt) xy-v)))

(defn pt->xy
  [^long p]
  (uv->xy (n->uv p)))

;; converts to a (u v 0). We coerce coords to int
(defn xy->pt-int
  "Converts an xy-point to a pt-point, with coords rounded to nearest int."
  [z]
  (pt (round-int (- (/ (first z) 2) (second z))) 0 (round-int (* (first z) sqrt3o3t2))))

(defn interpolate-list
  [point-a point-b ^long count]
  (let [delta (pt-div count (pt- point-b point-a))]
    (loop [made (list) c 0]
      (if (= c count)
        made
        (recur (cons (pt+ point-a (pt* c delta)) made) (inc c))))))

(def uv-origin (UV. 0 0))
(def pt-origin (long 0))

;; Use testing implies that 9 is the greatest radius output
;; for pt-radius

(def uv-radius-slow
  (fn [^UV p]
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
            (and (> u 0) (> v 0))   (+ u v)))))

(def uv-radius-fast
  (pt-lookup-table-optimize
   9
   uv-radius-slow))

(def pt-radius
  (optimize1-uv-n-pt
   (hexagonal-number 10)
   (fn [p] (n->uv (uv-radius-slow p)))))

(def pt-dist
  (fn
    [^long pta ^long  ptb]
    (pt-radius (pt- pta ptb))))

(defn uv-rot+60
  [^UV p]
  (n->uv (pt (:v p) 0 (- (:u p)))) )

(def pt-rot+60
  (optimize1-uv-n-pt
   (hexagonal-number 5)   
   uv-rot+60))

(defn uv-rot-60
  [^UV p]
  (n->uv (pt 0 (:u p) (:v p))))

(def pt-rot-60
  (optimize1-uv-n-pt
   (hexagonal-number 5)
   uv-rot-60))

(defn uv-div-4
  [^UV p]
  (uv* (/ 4) p))

(def pt-div-4
  (optimize1-uv-n-pt
   (hexagonal-number 5)
   uv-div-4))


  
  
  
  
  
  
  
  


