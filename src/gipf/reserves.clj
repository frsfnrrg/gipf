(ns gipf.core
  (:import (gipfj Reserves)))

(defrename ->Reserves `Reserves/makeReserves 1)
(defrename inc-reserves `Reserves/incReserves 2)
(defrename dec-reserves `Reserves/decReserves 2)
(defrename get-reserves `Reserves/getReserves 2)
