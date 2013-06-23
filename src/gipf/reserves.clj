(ns gipf.core
  (:import (gipfj Reserves)))

(definline ->Reserves [r p] `( Reserves/makeReserves ~r ~p))
(definline get-gipfs-on-board [r p] 
  `( Reserves/getGipfs ~r ~p))
(definline get-pieces-on-board [r p] 
  `( Reserves/getPieces ~r ~p))
(definline get-pieces-in-reserve [r p] 
  `( Reserves/getReserves ~r ~p))
(definline get-total-pieces [r p] 
  `( Reserves/getTotalPieces ~r ~p))
(definline reserve-delta
  [res player d-rpieces d-bpieces d-gipfs]
  `(do
     (println "change")
     (Reserves/change ~res ~player ~d-rpieces ~d-bpieces ~d-gipfs)))
(definline eqv-reserves [r p] `( Reserves/equiv ~r ~p))
(definline was-taken? [r p] `( Reserves/wasTaken ~r ~p))
(definline losing-reserve? [r p] `(Reserves/losingReserve ~r ~p))

(def null-reserves (->Reserves (/ Reserves/MAX_CAPACITY 2) (/ Reserves/MAX_CAPACITY 2)))
