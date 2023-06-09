;; github.com/rogerturner/.../untangling-sequences-lib.ss © 2019 Roger Turner
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; see *Notices* below for License and Contact links

#| HTM Untangling Sequences experiment (Scheme library)

  Indentation facilitates using an editor "Fold All" view for a file overview.

  |#
  
  #!chezscheme

(library (untangling-sequences-lib)

(export experiment)

(import
          (chezscheme)
          (parameters)                   ;; [ref: parameters]
          (frameworks htm-prelude)
          (frameworks htm-concept)
  (prefix (frameworks l2-l4-patch) l2l4:))
                                                                                            ;
(implicit-exports #f)

;; === Types (see htm-concept.ss) ===
;; SDR            = Listof Nat
;; FeatureX       = Range num-features
;; LocationX      = Range num-locations
;; Sensation      = (FeatureX, LocationX | #f)
;; Object         = Vector CCX->Sensation
;; Sequence       = Vector CCX->Sensation
;; Experience     = Object | Sequence

(define-record-type sensation (fields
  feature                                ;; SDRX
  location))                             ;; SDRX | #f

(define (experiment exp-args run-args)   ;; KWargs KWargs ->
  ;; train l2l4 patch on object/sequence experiences, test, summarize response
  (let* (
    (args (append run-args exp-args))    ;; run-args override exp-args
    (get  (lambda (key default)
            (let ((specified (assoc key args)))
              (if specified  (cdr specified)  default))))
                                                                                            ;
;; model and learning parameter defaults (overridable by experiment/run parameters)
    (num-cortical-columns         (get 'num-cortical-columns  1))
    (sensor-input-size            (get 'sensor-input-size    #f))
    (external-input-size          (get 'external-input-size  #f))
    (num-minicolumns              *mc/cc*)
    (ss4l4-c/mc                   (get 'ss4l4-c/mc     15))
    (ss4l23-c/mc                  (get 'ss4l23-c/mc     0))
    (p4-c/mc                      (get 'p4-c/mc        15))
    (l6-c/mc                      15)
    (num-input-bits               (get 'num-input-bits (fxmax 5 (int<- (* num-minicolumns 0.02)))))
    (initial-permanence           (get 'initial-permanence   (perm 0.51)))
    (connected-permanence         (get 'connected-permanence (perm 0.60)))
    (permanence-increment         (get 'permanence-increment (perm 0.1 )))
    (permanence-decrement         (get 'permanence-decrement (perm 0.02)))
    (activation-threshold         (get 'activation-threshold (int<- (* num-input-bits 0.8))))
    (min-threshold                (get 'min-threshold activation-threshold #;(int<- (* activation-threshold 0.8))))
    (reduced-basal-threshold      (get 'reduced-basal-threshold (int<- (* activation-threshold 0.6))))
    (predicted-segment-decrement  (get 'predicted-segment-decrement (perm 0.001)))
    (online-learning              (get 'online-learning      #f))
    (use-bursting-columns         (get 'use-bursting-columns #t))
    (prefer-nearby-minicol        (get 'prefer-nearby-minicol #t))
    (seed                         (get 'seed (+ 1 (modulo (time-second (current-time)) 1000))))
                                                                                            ;
;; setup network: override algorithm default parameters
;; [Hawkins etal 2017] => l4 is 10/150x16 (20/1024x16?), l2 is 40/4096
;; (scale to num-minicolumns - works for 100-400 minicolumns?)
    (l2-c/mc                      (if (< num-minicolumns 400)  16
                                      (+ 2 (fxdiv 2048 num-minicolumns))))
    (l2-sdr-size                  (int<- (* num-minicolumns l2-c/mc 0.008)))
    (l2-sample-size-proximal      (int<- (* 0.5 l2-sdr-size)))
    (l2-sample-size-distal        (* 1 l2-sample-size-proximal))
    (l2-overrides  (append (get 'l2-overrides '()) `(
      [initial-distal-permanence      . ,(perm 0.41)]
      [sample-size-proximal           . ,l2-sample-size-proximal]
      [min-threshold-proximal         . ,(int<- (* 0.5 l2-sample-size-proximal))]
      ;[predicted-inhibition-threshold . ,l2-sample-size-proximal]
      [sample-size-distal             . ,l2-sample-size-distal]
      [activation-threshold-distal    . ,(int<- (* 0.7 l2-sample-size-distal))]
      [sdr-size                       . ,l2-sdr-size]
      [min-sdr-size                   . ,(int<- (* 1 #;0.75 l2-sdr-size))]
      [predicted-inhibition-threshold . -1]
      [seed                           . ,seed]
      [online-learning                . ,online-learning])))
    (l4-overrides  (append (get 'l4-overrides '()) `(
      [destroy-threshold                  . ,(perm 0.31)]
      [use-bursting-columns-input         . ,use-bursting-columns]
      [sample-size                        . ,(int<- (* 1. num-input-bits))]
      [initial-permanence                 . ,initial-permanence]
      [connected-permanence               . ,connected-permanence]
      [permanence-increment               . ,permanence-increment]
      [permanence-decrement               . ,permanence-decrement]
      [activation-threshold               . ,activation-threshold]
      [min-threshold                      . ,min-threshold]
      [reduced-basal-threshold            . ,reduced-basal-threshold]
      [apical-predicted-segment-decrement . ,predicted-segment-decrement]
      [basal-predicted-segment-decrement  . ,predicted-segment-decrement])))
    (patch 
      (l2l4:make-patch num-cortical-columns num-minicolumns
        l2-c/mc ss4l4-c/mc ss4l23-c/mc p4-c/mc l6-c/mc
        l2-overrides l4-overrides))
                                                                                            ;
;; default experiment parameters (overridable)
    (figure                       (get 'figure               'z))
    (num-objects                  (get 'num-objects          50))
    (num-points                   (get 'num-points           10))
    (num-sequences                (get 'num-sequences        50))
    (seq-length                   (get 'seq-length           10))
    (num-features                 (get 'num-features        100))
    (num-locations                (get 'num-locations       100))
    (num-repetitions              (get 'num-repetitions       5))
    (random-seq-location          (get 'random-seq-location  #f))
    (location-per-sequence        (get 'location-per-sequence #f))
    (intersperse-noise            (get 'intersperse-noise     0))
    (superimpose-sequence         (get 'superimpose-sequence #f))
    (interleave-training          (get 'interleave-training  #f))
                                                                                            ;
;; create the sequences and objects
    (location-bits  (if external-input-size  num-input-bits
                        (fxmax 8 (int<- (* num-minicolumns l6-c/mc 0.01)))))
    (random-sdr (lambda (size bits)      ;; Nat Nat -> ( -> SDR)
        ;; produce function to generate SDR of size with sparsity bits/size
        (lambda ()
          (sort fx<? (u32-sample (iota size) bits)))))
    (location-sdr (lambda                ;; Nat Nat Nat -> ( -> SDR)
                    (ncols ncells/col bits)
        ;; produce function to generate SDR with sparsity bits/(ncols * ncells/col)
        ;; max 1 bit/minicol (sparse block-code Frady etal 2020)
        (lambda ()
          (let ([col-sdr (sort fx<? (u32-sample (iota ncols) bits))])
            (map (lambda (colx)
                (fx+ (fx* colx ncells/col) (random ncells/col)))
              col-sdr)))))
    (build-distinct (lambda (n b ->sdr)  ;; Nat Nat (-> SDR) -> (Vector Nat->SDR)
        ;; produce n vector of b on-bit SDRs with minimum overlaps (no more than b/5 bits)
        (let ([v (make-vector n)])
          (do ([vx 0 (fx1+ vx)]) ((fx=? vx n) v)
            (let try1 ([overlap 0])
              (if (fx>? overlap (fxdiv b 5))  (error #f "can't generate distinct features or locations")
                (let try2 ([tries 0])
                  (if (fx>? tries 20) (try1 (fx1+ overlap))
                    (let ([sdr (->sdr)])
                      (do ([vx- 0 (fx1+ vx-)])
                          ((or (fx=? vx- vx)
                               (fx>? (length (intersect1d (vector-ref v vx-) sdr)) overlap))
                            (if (fx=? vx- vx)  (vector-set! v vx sdr)
                              (try2 (fx1+ tries))))))))))))))
    (feature-sdr (lambda (sis nmc bits)  ;; Nat Nat Nat -> ( -> SDR)
        ;; produce function to generate SDR of size with sparsity bits/size
        (lambda ()
          (sort-unique! (map (lambda (se)
                            (fxmod se nmc))
                          (u32-sample (iota sis) bits))))))
    (feature-pool                        ;; Vector CCX->(Vector FeatureX->SDR)
      (build-vector num-cortical-columns (lambda _
        (build-distinct num-features num-input-bits #;(random-sdr num-minicolumns num-input-bits)
          (if sensor-input-size  (feature-sdr sensor-input-size num-minicolumns num-input-bits)
            (random-sdr num-minicolumns num-input-bits))))))
    (location-pool                       ;; Vector CCX->(Vector LocationX->SDR)
      ;; distinct locations for objects and sequences
      (build-vector num-cortical-columns (lambda _
        (build-distinct (* 2 num-locations) location-bits
          (if external-input-size  (random-sdr external-input-size location-bits)
              (location-sdr num-minicolumns l6-c/mc location-bits))))))
    (create-random-experiences           ;; Nat Nat Nat -> Experiences
      (lambda (num-exps num-sensations num-locations)
        ;; produce Object experiences, or Sequence experiences when num-locations is zero
        (build-vector num-exps (lambda _
          ;; locations are distinct but features can be repeated
          (let ([locations (if (positive? num-locations)
                             (list->vector (u32-sample (iota num-locations) num-sensations))
                             (make-vector num-sensations #f))]
                [features  (build-vector num-sensations (lambda _ (random num-features)))])
            (build-vector num-sensations (lambda (sx)
                (make-sensation (vector-ref features sx) (vector-ref locations sx)))))))))
    (sequences                           ;; Vector SequenceX->Experience
      (create-random-experiences num-sequences seq-length 0))
    (objects                             ;; Vector ObjectX->Experience
      (create-random-experiences num-objects num-points num-locations))
                                                                                          ;
    ;; Stats is (Vector ExperienceX->(Vector SensationX->(KeyListOf (Vector CCX->Number|{Nat}))))
    (stats  (build-vector (max num-objects num-sequences) (lambda _ 
                (build-vector (if (eq? figure 'f6)  (* 10 num-points)
                                  (max num-points seq-length))
                              (lambda _ (list))))))
    (display-timing (get 'display-timing  #t))
    (train-keys     (get 'train-keys     '()))
    (test-keys      (get 'test-keys      '()))
    (test-keys      (filter (lambda (key)
                        (let ([k2 (substring (symbol->string key) 0 2)])
                          (case k2
                            [("TM") (positive? ss4l4-c/mc)]
                            [("TX") (positive? ss4l23-c/mc)]
                            [("l4") (positive? p4-c/mc)]
                            [else #t])))
                      test-keys))
    (f6-objectxs    #f)
    )

(define (have-sensations-of              ;; Experience Bool . (Nat -> ) ->
          experience learn . report)
  ;; feed each sensation of experience to model, optional report after each sensation
  (let* (
      [object?    (sensation-location (vector-ref experience 0))]
      [ns         (vector-length experience)]
      ;; shuffle object sensations except on final training presentation
      [sensations (if (and object? (null? report))
                      (vector-sample experience ns)
                      experience)]
      ;; for each sensation of object, feature+location per cc
      ;; for sequences, all ccs get same feature, different random location
      #;[seq-locations
              (build-vector num-cortical-columns (lambda (ccx)
                  (vector-ref (vector-ref location-pool ccx)
                              (+ num-locations (random num-locations))))) ])
    (do ([sx 0 (fx1+ sx)]) ((fx=? sx ns))
      (let (
          [features
            (build-vector num-cortical-columns (lambda (ccx)
                  (vector-ref (vector-ref feature-pool ccx)
                    (sensation-feature (vector-ref sensations sx))))) ]
          [locations
            (build-vector num-cortical-columns (lambda (ccx)
              (if object?
                (vector-ref (vector-ref location-pool ccx)
                  (sensation-location (vector-ref sensations sx)))
                (vector-ref (vector-ref location-pool ccx)
                    (+ num-locations (random num-locations)))))) ])
        (l2l4:compute patch features locations learn)
        (unless (null? report)
          ((car report) sx features))))))
                                                                                            ;
(define (get-stats-for keys)             ;; {Symbol} [{{Nat}}]-> (Alistof (CCVectorOf {Nat}))
  ;; produce alist of stats for keys
  (define (lengths ->cells ->layers) ;; (Layer -> {CellX}) (Patch -> CCVectorOf Layer) -> (vectorof Nat)
    ;; produce lengths of cells of layer for each cc
    (vector-map length
      (vector-map ->cells (->layers patch))))
  (map (lambda (key)
      (cons key
        (case key
          [(l2t)   (vector-map (lambda (l) (l2l4:get-l2-active-cells l)) (l2l4:patch-l2s patch))]      
          [(l2a)   (vector-map (lambda (l) (l2l4:get-l2-active-cells l)) (l2l4:patch-l2s patch))]      
          [(l4lnp) (lengths    (lambda (l) (l2l4:get-predicted-cells        l 'p4    )) l2l4:patch-l4s)]
          [(l4lpa) (lengths    (lambda (l) (l2l4:get-predicted-active-cells l 'p4    )) l2l4:patch-l4s)]
          [(l4ac)  (vector-map (lambda (l) (l2l4:get-active-cells           l 'p4    )) (l2l4:patch-l4s patch))]
          [(l4pa)  (vector-map (lambda (l) (l2l4:get-predicted-active-cells l 'p4    )) (l2l4:patch-l4s patch))]
          [(TMlnp) (lengths    (lambda (l) (l2l4:get-predicted-cells        l 'ss4l4 )) l2l4:patch-l4s)]
          [(TMlpa) (lengths    (lambda (l) (l2l4:get-predicted-active-cells l 'ss4l4 )) l2l4:patch-l4s)]
          [(TMac)  (vector-map (lambda (l) (l2l4:get-active-cells           l 'ss4l4 )) (l2l4:patch-l4s patch))]
          [(TMpa)  (vector-map (lambda (l) (l2l4:get-predicted-active-cells l 'ss4l4 )) (l2l4:patch-l4s patch))]
          [(TXlnp) (lengths    (lambda (l) (l2l4:get-predicted-cells        l 'ss4l23)) l2l4:patch-l4s)]
          [(TXlpa) (lengths    (lambda (l) (l2l4:get-predicted-active-cells l 'ss4l23)) l2l4:patch-l4s)]
          [(TXac)  (vector-map (lambda (l) (l2l4:get-active-cells           l 'ss4l23)) (l2l4:patch-l4s patch))]
          [(TXpa)  (vector-map (lambda (l) (l2l4:get-predicted-active-cells l 'ss4l23)) (l2l4:patch-l4s patch))]
          )))
    keys))
                                                                                            ;
(define (train-all es)                   ;; Experiences ->
  ;; train on each e num-repetitions times
  (do-with-progress (vector-length es)
    (lambda (ex)
      (let ([e (vector-ref es ex)])
        (do ([r 0 (fx1+ r)]) ((fx=? r (fx1- num-repetitions))
                              (have-sensations-of e #t
                                (lambda (sx features)
                                  (vector-set! (vector-ref stats ex) sx
                                    (append (get-stats-for train-keys)
                                            (vector-ref (vector-ref stats ex) sx)
                                            '())))))            
          (have-sensations-of e #t)
          (unless (sensation-location (vector-ref e 0))
            (l2l4:reset-seq patch))) ;; reset TM between presentations of sequence
        (when (positive? intersperse-noise)
          (let ((noise (vector-ref (create-random-experiences 1 seq-length 0) 0)))
            (do ((_ 0 (fx1+ _))) ((fx=? _ intersperse-noise))
              (have-sensations-of noise #t))))
        (l2l4:reset patch)))))       ;; reset after each object
                                                                                            ;
(define (train-all-interleaved)          ;; ->
  ;; train objects & sequences
  ;; reset l2 only before first encounter of each experience
  ;; reset sequence memory (ss4l4) before each sequence
  (assert (fx=? num-objects num-sequences))
  (let ()
    (define (train-all-items max-tries)
      (define (train-one-item seq/obj ex tries pop)
        ;; train-one-item
        (when (fx=? tries num-repetitions)
          (l2l4:reset-l2 patch))
        (let train-up-to ([tx tries])
          (when (fx>? tx 8)
            (have-sensations-of (vector-ref seq/obj ex) #t))
          (unless (or (fxzero? tx)
              (fx<? (fxmax 0 (fxdiv (fx* num-cortical-columns 3) 4))
                    (vector-fold-left (lambda (ncc l)
                        (if (fx=? num-input-bits (length (l2l4:get-active-cells l pop)))
                          (fx1+ ncc)  ncc ))
                      0
                      (l2l4:patch-l4s patch))))
            (have-sensations-of (vector-ref seq/obj ex) #t)
            (train-up-to (fx1- tx)))))
      ;; train-all-items
      (do ([ex 0 (fx1+ ex)]) ((fx=? ex num-objects))
        (l2l4:reset-l4 patch)
        (train-one-item sequences ex max-tries 'ss4l4)
        (l2l4:reset-l4 patch)
        (train-one-item objects   ex max-tries (if (fxpositive? p4-c/mc) 'p4 'ss4l23))))
    (do ([max-tries num-repetitions
                    (if (fx>? max-tries 16) (fxdiv max-tries 2) (fx1- max-tries))])
        ((fxzero? max-tries))
      (train-all-items max-tries))
    (train-all-items 1)))
                                                                                            ;
(define (infer-all es)                   ;; Experiences ->
  ;; test each experience and save stats for each sensation
  (do-with-progress (vector-length es)
    (lambda (ex)
      (have-sensations-of (vector-ref es ex) #f
        (lambda (sx features)
          (vector-set! (vector-ref stats ex) sx
            (append (get-stats-for test-keys)
                    (vector-ref (vector-ref stats ex) sx)
                    '()))))
      (l2l4:reset patch))))
                                                                                            ;
(define (infer-switching)                ;; ->
  ;; test mix of objects & sequences
  (for-each
    (lambda (item-type ix)
      (have-sensations-of
        (vector-ref
          (if (eq? item-type 'seq) sequences  objects) (vector-ref f6-objectxs ix))
        #f
        (lambda (sx features)
          (vector-set! (vector-ref stats 0) (fx+ (fx* num-points ix) sx)
            (append (get-stats-for test-keys)
                    (vector-ref (vector-ref stats 0) (fx+ (fx* num-points ix) sx))
                    '()))))
      (l2l4:reset patch))
    '(seq obj seq obj seq seq obj seq obj obj) (iota 10)))
                                                                                            ;
(define (f6->plot ccx)                   ;; CCX -> (List (Key . Param) ... (Key . (SensListof #(Number))) ... )
  ;; restructure stats to plot order
  ;; stats is (ExpVectorOf (SensVectorOf (Listof [Key . (Vector CCX->(Listof Number))])))
  ;; output   (CClistof (List (Key . Param) ... (Key . (SensListof #(Number))) ... ))
  ;;    stats                   ccs
  ;;     #*                     (*)
  ;;  experience            keyed-plotvs
  ;;     #*                  /        \
  ;;  sensation           params   key/plotvs
  ;;     (*)               (*)         (*)
  ;;  key/cc*data    (key . param)  (key . senss)
  ;;    /   \                               (*)
  ;;  key . ccvec                          ccvec
  ;;         #*                             #*
  ;;        data                           data
  ;;         (*)                          o    o
  ;;       number                     number (numbers)
  (define (->plot-data make-data)    ;; (Key )
    (map (lambda (test-key)          ;; map test keys
          [cons (case test-key
              [(l4pa l4lpa) "l4pa" ]
              [(TMpa TMlpa) "TMpa" ]
              [(TXpa TXlpa) "TXpa" ] )
            [list (vector->list
                (vector-map (lambda (key-datavecs)  ;; map SensVector
                    (let ([ccdatavec (cdr (assq test-key key-datavecs))])  ;; find key in alist
                      (make-data ccdatavec)))
                  (vector-ref stats 0)))  ;; first exp
              ] ] )
        test-keys))
  [cons [list "figure" (symbol->string figure)]
    [cons [list "using"  (cons `[num-minicolumns . ,num-minicolumns] run-args)]
      (->plot-data (lambda (ccdatavec)
          (vector (vector-ref ccdatavec ccx)))) ] ] )
                                                                                            ;
(define (H3b->plot ccx)                  ;; CCX -> { (Key . {}) }
  (define (counts key)
    (filter positive? (vector->list
        (vector-fold-left (lambda (acc sensation)
            (let ([ccvec (cdr (assq key sensation))])
              (for-each (lambda (colx)
                  (vector-set! acc colx (fx1+ (vector-ref acc colx))))
                (vector-ref ccvec ccx)))
            acc)
          (make-vector (l2l4:get-l2-number-of-cells (vector-ref (l2l4:patch-l2s patch) 0)) 0)
          (vector-ref stats 0)))))
  (append
    (map (lambda (key)
        [list (symbol->string key)
          (vector-ref
            (cdr (assq key (vector-ref (vector-ref stats 0) 0)))
            ccx) ])
      train-keys)
    (map (lambda (key)
        [list (symbol->string key)
                                   ;; union of sensations
              (vector-fold-left (lambda (acc sensation)
                  (let ([ccvec (cdr (assq key sensation))])
                    (union1d acc (vector-ref ccvec ccx))))
                '()
                (vector-ref stats 0))])
      test-keys)
    (map                   ;; number of sensations/cell for each stat
      (lambda (key)
        [list (string-append (symbol->string key) "c")
              (counts key) ])
      test-keys))
  )
#|      
(let* ([target  (length ts)]
       [match   (let loop ([m 1])
                  (cond
                    [(> m 10) 10]
                    [(<= (- target 1) (length (filter (lambda (x) (> x m)) xs)) (+ target 1)) m]
                    [else (loop (+ m 1))]))]
|#
  #;
  (let ([target (length l2t)])
    (let loop ([m 1])
      (cond
        [(> m 10) 10 ]
        [(<=
          (- target 1)
          (length
            (filter (lambda (x)
                (> x m))
              l2ac))
          (+ target 1))  m ]
        [else (loop (+ m 1)) ])))
                                                                                            ;
  (random-seed seed)
;; run experiment: 1 train network
  (if interleave-training
    (train-all-interleaved)
    (begin
      (train-all objects)
      (unless superimpose-sequence
        (train-all sequences))))
;;                 2 save objects/sequences and trained stats (f6)
  (when (and (eq? figure 'f6) (positive? num-repetitions))
    (set! f6-objectxs (vector-sample (build-vector num-objects values) 10))
    (for-each
      (lambda (item-type ix)
        (have-sensations-of
          (vector-ref
            (if (eq? item-type 'seq) sequences  objects) (vector-ref f6-objectxs ix))
          #t
          (lambda (sx features)
            (vector-set! (vector-ref stats 0) (fx+ (fx* num-points ix) sx)
              (get-stats-for train-keys)))))
      '(seq obj seq obj seq seq obj seq obj obj) (iota 10)))
;;                 3 reset and run inference
  (l2l4:reset patch)
  (when (positive? num-repetitions)
    (case figure
      [(f6)
        (infer-switching) ]
      [(f3b F3b H3b)
        (have-sensations-of (vector-ref objects 0) #f
          (lambda (sx features)
            (vector-set! (vector-ref stats 0) sx
              (append (get-stats-for test-keys)
                      (vector-ref (vector-ref stats 0) sx)
                      '())))) ]
      [else
        (infer-all objects)
        (infer-all sequences) ] ))
;;                 4 save plot data for each cc
  (with-output-to-file "experiment.data" (lambda ()
      (case figure
        [(f6)
          (write (map f6->plot (iota num-cortical-columns))) ]
        [else
          (write (map (lambda (ccx)
              [cons [list "figure" (symbol->string figure)]
                [cons [list "using"  (cons `[num-minicolumns . ,num-minicolumns] run-args)]
                  (H3b->plot ccx) ] ] )
            (iota num-cortical-columns))) ]))
    'truncate)
;;                 5 display statistics
  (when display-timing
    (for-each display `(
        ,(symbol->string figure) " "
        ,@(if (assoc 'seed run-args)  run-args
              (cons `(seed . ,seed) run-args))
        ,(if (zero? (vr0@))  ""  (vr0@))
        "\n"))
    (l2l4:print-statistics patch))))

)

#| *Notices*

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with this program.  If not, see <https://www.gnu.org/licenses/>.
  
  License: <https://www.gnu.org/licenses/agpl-3.0.txt>
  Contact: <https://github.com/rogerturner/HTM-scheme/issues/new/choose>  |#
