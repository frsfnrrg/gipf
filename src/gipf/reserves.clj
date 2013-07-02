(ns gipf.core
  (:import (gipfj Reserves)))

(definline ->Reserves [r p g] `( Reserves/makeReserves ~r ~p ~g))
(definline get-gipfs-on-board [r p] 
  `(Reserves/getGipfs ~r ~p))
(definline get-pieces-on-board [r p] 
  `(Reserves/getPieces ~r ~p))
(definline get-pieces-in-reserve [r p] 
  `(Reserves/getReserves ~r ~p))
(definline get-total-pieces [r p] 
  `(Reserves/getTotalPieces ~r ~p))
(definline reserve-delta [res player d-rpieces d-bpieces d-gipfs]
  `(Reserves/change ~res ~player ~d-rpieces ~d-bpieces ~d-gipfs))
(definline eqv-reserves [r p]
  `(Reserves/equiv ~r ~p))
(definline was-taken? [r p]
  `(Reserves/wasTaken ~r ~p))
