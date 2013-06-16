(ns gipf.core-test
  (:use clojure.test
        gipf.core))

(defn check [k] 
  (gipf.core/or-coll (fn [u] (not= u (gipf.core/pt->n (gipf.core/n->pt u))) (range k))))

(deftest geo-point-int
  (testing "Is the N<->pt mapping one to one?"
    (let []
      (is (= (check 5) true)))))

