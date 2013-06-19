(ns gipf.core
  (:import (gipfj Reserves)))

(defrename ->Reserves `Reserves/makeReserves 2)
(defrename inc-reserves `Reserves/incReserves 2)
(defrename dec-reserves `Reserves/decReserves 2)
(defrename get-reserves `Reserves/getReserves 2)
(defrename inc-gipfs `Reserves/incGipfs 2)
(defrename dec-gipfs `Reserves/decGipfs 2)
(defrename get-gipfs `Reserves/getGipfs 2)
