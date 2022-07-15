(defpackage :layoup
  (:use :cl)
  (:export
   :pos
   :pos-p
   :make-pos
   :copy-pos
   :pos-finger
   :pos-col
   :pos-row
   :pos-x
   :pos-y
   :corpus
   :new-corpus
   :add-to-corpus
   :make-corpus
   :corpus-chars
   :corpus-bigrams
   :corpus-trigrams
   :corpus-skipgrams
   :calculate-metrics
   :metric
   :make-metric
   :copy-metric
   :metric-p
   :metric-name
   :metric-fn
   :metric-result
   :metric-list
   :metric-p
   :make-metric-list
   :copy-metric-list
   :metric-list-bigraphs
   :metric-list-trigraphs
   :finger
   :make-finger
   :copy-finger
   :finger-hand
   :finger-finger
   :finger-p
   :LT :LI :LM :LR :LP :RT :RI :RM :RR :RP
   :finger-list
   :ansi-pos
   :matrix-pos
   :pos-to-key
   :analyze-keys
   :key-matrix
   :swap-keys
   :add-file-to-corpus
   :keys-total)
  (:local-nicknames (:alex :alexandria)))
(in-package :layoup)
