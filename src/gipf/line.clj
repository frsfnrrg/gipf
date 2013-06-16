(ns gipf.core)


;
;(defn uv-get-line-limit-point
;  ;; best would be pure arithmatic version...
;  [^UV pos ^UV vecback]
;  (loop [cur pos]
;    (let [next (uv+ cur vecback)]
;      (if (= (uv-radius-fast next) 4)
;        cur
;        (recur next)))))
;
;
;(defn optimize-1-ring-uv-n-pt
;  "Optimizes a UV x UV -> UV to be a N X N -> N"
;  [^long size func]
;
;  (let [table (make-array java.lang.Long/TYPE (inc (* 6 size)))]
;    (doseq [r (range 1 7 1)
;            p (range size)]
;      (aset-long table
;                 (+ r (* 6 p))
;                 (uv->n (func (n->uv p) (n->uv r)))))
;     
;    (fn [^long p ^long r]
;      (aget ^longs table
;            (unchecked-add r
;                           (unchecked-multiply p 6)))) ))
;
;(def get-line-limit-point
;  "You will get nonsense if the second arg is not in the unit hex"
;  (optimize-1-ring-uv-n-pt
;   (hexagonal-number 4)
;   uv-get-line-limit-point))
