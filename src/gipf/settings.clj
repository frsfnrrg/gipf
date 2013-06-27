(ns gipf.core)

(let [dia (atom {:move-newlines true
                 :move-numbers false
                 :hist-analysis true
                 :transp-analysis true
                 :match-result true
                 :total-time true
                 :incremental-time true
                 :moves-available false
                 :reserve-status false
                 :board-snapshot false
                 :rank-value false
                 :pre-rank-value false
                 :screen-display true
                 :evaluation-count true
                 :equal-moves true
                 :pre-calc-message false})]
  (defn set-diagnostic-level!
    [key ^Boolean on]
    (swap! dia (fn [a] (assoc a key on))))
  (defn get-diagnostic-level
    [key]
    (get @dia key))
  (defmacro ond
    [key & actions]
    `(when (get-diagnostic-level ~key)
       ~@actions)))
