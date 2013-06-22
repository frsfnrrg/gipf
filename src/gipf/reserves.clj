(ns gipf.core)

(defrename ->Reserves `Reserves/makeReserves 2)
(defrename inc-reserves `Reserves/incReserves 2)
(defrename dec-reserves `Reserves/decReserves 2)
(defrename get-reserves `Reserves/getReserves 2)
(defrename inc-gipfs `Reserves/incGipfs 2)
(defrename dec-gipfs `Reserves/decGipfs 2)
(defrename get-gipfs `Reserves/getGipfs 2)
(defrename eqv-reserves `Reserves/equiv 2)

(def null-reserves (->Reserves 0 0))
