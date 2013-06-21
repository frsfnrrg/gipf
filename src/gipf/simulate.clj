(ns gipf.sim)

(set! *warn-on-reflection* true)

(load "core")

(let []
  (gipf.core/runSimulation :normal))
