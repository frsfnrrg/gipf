;; check if *ran* is defined: if no, define it, call close
(ns gipf.repl)

(set! *warn-on-reflection* true)

(gipf.core/runGUI)

"Ola!"
