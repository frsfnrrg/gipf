;; check if *ran* is defined: if no, define it, call close
(ns gipf.repl)

(set! *warn-on-reflection* true)

(load "core")

(gipf.core/runGUI)

"Ola!"
