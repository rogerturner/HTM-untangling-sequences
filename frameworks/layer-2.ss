;; github.com/rogerturner/.../layer-2.ss © 2019 Roger Turner
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; see *Notices* below for License and Contact links

#| HTM-scheme Layer-2 framework

Temporal memory timestep:
  previous activity -> context/feedback input -> depolarization (apical -> long)
  => active basal/apical segments,
     matching basal/apical segments,
     apical history,
       (per activation-threshold, min-threshold)
       
Layer timestep:
  active basal/apical segments 
  
context -> L4 => L4 depolarization, inhibition
  feedforward -> L4 => L4 activity (~inhibited)
    context -> L2/3 => L2/3 depolarization, inhibition
      L4 activity -> L2/3 => L2/3 activity (~inhibited)
    
    input + connected reaching active-threshold -> active segment   
    input reaching min-threshold -> matching segment  
     
    basal + apical active in group -> depolarize more -> fire first, inhibit rest
    basal active in group -> depolarize -> inhibit rest
    (apical -> depolarize -> inhibit)?
    
  feedforward + depolarization ->
    -> inhibit rest of group (ignore feedforward)
    active/matching segments => learning:
      -> reinforce (context correct)
      -> add synapses (to feedforward, context, feedback?)
      -> add new segments

  depolarization without feedforward
    -> punish (context incorrect)

  feedforward without depolarization in group
    -> "bursting" activate all in group
    
"Object layer":
  hallmark is persistent object representation with changing feedforward input
  L4 -> L2/3 connections unclear (see refs)
  use L4 -> "basal", L2/3 -> "proximal" + apical
    active apical segment -> predicted cell
    
column pooler uses long depolarization (?), inter-cc connections
  inter-cc connections have to be seeded
  previous activity -> context/feedback input -> depolarization
  ff input + depolarization ->

  [Packer & Yuste 2011 Dense, Unspecific Connectivity of Neocortical Parvalbumin-Positive 
  Interneurons- A Canonical Microcircuit for Inhibition?]
  Interneurons connect to every PC within 100-200µm (ie adjoining minicols, not layers)
  
  [Szegedi etal 2017 High-Precision Fast-Spiking Basket Cell Discharges during Complex Events 
  in the Human Neocortex]

  [Rikhye etal 2019 Memorize-Generalize]:
  neighboring neurons in Layer 2/3 derive their receptive fields from common feed-forward input
  from neurons in Layer 4 and thus would redundantly encode the same stimulus features
  
  [Yen, Baker, & Gray, 2007] Second, although these neurons share a common receptive field,
  they respond to different instances of the same sensory information (Vinje & Gallant, 2000)

  [Behabadi etal 2012 Location-Dependent Excitatory Synaptic Interactions in
  Pyramidal Neuron Dendrites, Figure 8]:
  > EPSP time-course analysis of L3 model neuron suggests that "modulatory" long-distance 
  > horizontal connections terminate proximally and vertical L4 "driver" inputs terminate 
  > distally.
  
  [Little & Carter 2012 Subcellular Synaptic Connectivity of Layer 2 Pyramidal
  Neurons in the Medial Prefrontal Cortex]
    L2->L2 proximal/distal depends on source area
  
  [Yoshimura etal 2000 Properties of Horizontal and Vertical Inputs to Pyramidal Cells 
  in the Superficial Layers of the Cat Visual Cortex]
    L2/3->L2/3 -> apical dendrites
  
  [Lubke & Feldmeyer 2010 The Axon of Excitatory Neurons in the Neocortex: Projection 
  Patterns and Target Specificity]
  
  [Lubke etal 2003 Morphometric Analysis of the Columnar Innervation Domain of Neurons 
  Connecting Layer 4 and Layer 2/3 of Juvenile Rat Barrel Cortex]

|#

  #!chezscheme

(library (frameworks layer-2)
                                                                                            ;
;; using column-pooler - just a shim

(export
  (rename
    (make-cp   make-l2)
    (cp-reset  l2-reset)
    (number-of-cells  l2-number-of-cells)
    (get-active-cells l2-active-cells)
    (cp-n-sdrs        l2-n-sdrs)
    (cp-proximal-filter-set! l2-proximal-filter-set!)
    (cp-distal-filter-set! l2-distal-filter-set!))
  l2-compute
  l2-print-statistics)
                                                                                              ;
  (import
    (except (chezscheme) reset)
            (parameters)
            (frameworks htm-prelude)
            (frameworks htm-concept)
            (algorithms column-pooler))
            
(define (l2-compute l2                   ;; L2 {Source} {{Source}} {Source} ?? Boolean {Source} ?? ->
    feedforward-input                    ;; l4-active-cells (this cc)
    ; no location input
    context-input                        ;; l2-active-cells (all ccs)
    ; no feedback input
    feedforward-growth-candidates        ;; l4-learning: predicted-active
    context-growth-candidates            ;; (cp will use context-input)
    learning
    predicted-input                      ;; l4-predicted-
    bursting-columns)
  ;; cp compute: Numenta compatible interface
  (cp-compute l2
    feedforward-input
    context-input
    feedforward-growth-candidates
    learning
    predicted-input))

#|
;;(using tm-neuron-layer)

(export
  make-l2
  l2-compute
  l2-reset
  (rename
    (number-of-cells l2-number-of-cells)
    (get-active-cells l2-active-cells))
  l2-print-statistics)
  
(import
  (except (chezscheme) reset)
          (parameters)
          (frameworks htm-prelude)
          (frameworks htm-concept)
  (rename (algorithms tm-pyramidal-layer)
    (tm-proximal-input-size number-of-inputs)))
                                                                                            ;
  (define (layer-2) 'ok)                    ;; (called to trigger tests)
                                                                                            ;
  ;; === Layer record ===
                                                                                            ;
(define l2-defaults `(                   ;; (listof KWarg) [cf __init__ in column_pooler.py]
    [input-width                    . 0]
    [lateral-input-widths           . ()]
    [cell-count                     . 4096]
    [sdr-size                       . 40]
    [online-learning                . #f]
    [max-sdr-size                   . 0]
    [min-sdr-size                   . 0]
    [seed                           . 42]
    [n-sdrs                         . 0]
    ))
                                                                                            ;
(define-record-type l2                   ;; extends pyramidal-temporal-memory tm
  (parent tm)
  (fields
    input-width
    lateral-input-widths
    cell-count
    sdr-size
    online-learning
    max-sdr-size
    min-sdr-size
    seed
    (mutable n-sdrs)
    )
(protocol #;(make-l2 l2-overrides)       ;; {KWarg} -> L2
  (lambda (pargs->new)
    (lambda (tm+l2-args)
      (define (make-proximal-at)    ;; Nat -> AxonTree
        ;; initialisation for L2: empty AxonTree
        (make-at 0))
      (define (proximal->cols at proximal-input)  ;; AxonTree {Source} -> {ColX}
        ;; produce minicolumns which could be activated by proximal-input
        (iota *mc/cc*))
      (define (calc-predicted-cells tm)  ;; TM -> {CellX}
        ;; produce "predicted" (expected) cells for pyramidal-cell-layer algorithm
        (tm-active-cells tm))
      (let* (
          [l2 (apply (pargs->new tm+l2-args)
                     (key-word-args tm+l2-args l2-defaults))]
          [tm+l2-args  (if (fxpositive? (l2-max-sdr-size l2))  tm+l2-args
                        (cons `[max-sdr-size . ,(l2-sdr-size l2)] tm+l2-args))]
          [tm+l2-args  (if (fxpositive? (l2-min-sdr-size l2))  tm+l2-args
                        (cons `[min-sdr-size . ,(l2-sdr-size l2)] tm+l2-args))]
          [cells-per-column    (fxdiv (l2-cell-count l2) (tm-column-count l2))]
          [proximal-input-size (l2-input-width l2)]
          [tm+l2-args (append `(
                             [cells-per-column     . ,cells-per-column]
                             [proximal-input-size  . ,proximal-input-size]
                             [layer                . 1]
                             [use-bursting-columns-input . #t]
                             [make-proximal-at     . ,make-proximal-at]
                             [proximal->cols       . ,proximal->cols]
                             #;
                             [calc-predicted-cells . ,calc-predicted-cells])
                           tm+l2-args)])
        (apply (pargs->new tm+l2-args)
               (key-word-args tm+l2-args l2-defaults))
               
              )))))
              
(define (l2-reset l2)                    ;; L2 ->
  ;; when learning this signifies we are to learn a unique new object
  (tm-active-cells-set! l2 (list)))
#;
(define (l2-compute l2
    feedforward-input                    ;; l4-active-cells (this cc)
    ; no location input
    context-input                        ;; (CCvecOf l2-active-cells)
    ; no feedback input
    feedforward-growth-candidates        ;; l4-learning: predicted-active
    context-growth-candidates            ;; (cp will use active-cells)
    learning
    predicted-input                      ;; l4-predicted-?
    bursting-columns)
  ;; cp compute: Numenta compatible interface
  (compute l2
    feedforward-input
    (let ([ncc (vector-length context-input)]
          [thiscc (cp-cortical-column l2)])
      (do ([ccx 0 (fx1+ ccx)]            ;; lateral-inputs: active-cells
            [laterals (list)             ;; in other ccs l2s
             (if (fx=? ccx thiscc)  laterals
                 (cons (vector-ref context-input ccx) laterals))])
          ((fx=? ccx ncc) laterals)))
    feedforward-growth-candidates
    learning
    predicted-input))

(define  l2-compute (case-lambda         ;; L2 {CellX} {CellX} {CellX} Boolean {CellX} ->
[(l2 feedforward-input learn bursting-cols)            ;; L2 {CellX} Boolean ->
  (l2-compute l2 feedforward-input '() '() learn '() bursting-cols) ]
[(l2  feedforward-input                  ;; l4-active-cells (this cc)
      ; no location input
      context-input                      ;; (CCvecOf l2-active-cells)
      ; no feedback input
      feedforward-growth-candidates      ;; l4-learning: predicted-active
      context-growth-candidates          ;; (CCvecOf l2-active-cells)
      learning
      predicted-input                    ;; l4-predicted-?
      bursting-columns)
  ;; run one time step of the column pooler algorithm

      (tm-depolarize-cells l2
          feedforward-input              ;; active cells pre-synaptic to proximal segments
          context-input                  ;; active cells pre-synaptic to basal segments
          '()                            ;; active cells pre-synaptic to apical segments
          #t)                            ;; learning

  (let ([feedforward-growth-candidates
          (if (null? feedforward-growth-candidates) feedforward-input
              feedforward-growth-candidates)])
    (if (not learn)                      ;; inference step
      (compute-inference-mode l2 feedforward-input context-input)
      (if (not (l2-online-learning l2))  ;; learning step
        (compute-learning-mode l2 feedforward-input context-input 
          feedforward-growth-candidates bursting-columns)
        #f 
        #;
        (cond                            ;; online learning step
          [(fx>? (length predicted-input) (l2-predicted-inhibition-threshold l2))
            (let ([predicted-active-input
                    (intersect1d feedforward-input predicted-input)]
                  [predicted-growth-candidates
                    (intersect1d feedforward-growth-candidates predicted-input)])
              (compute-inference-mode l2 predicted-active-input distal-input lateral-inputs)
              (compute-learning-mode l2 predicted-active-input distal-input lateral-inputs
                predicted-growth-candidates feedforward-growth-candidates)) ]
          [(not (fx<=? (l2-min-sdr-size l2)
                       (length (l2-active-cells l2))
                       (l2-max-sdr-size l2)))
            ;; if no single representation, try to infer one before attempting to learn
            (compute-inference-mode l2 feedforward-input distal-input lateral-inputs)
            (compute-learning-mode l2 feedforward-input distal-input lateral-inputs
              feedforward-growth-candidates) ]
          [else 
            ;; no predicted input and single SDR, extend that representation
            (compute-learning-mode l2 feedforward-input distal-input lateral-inputs
              feedforward-growth-candidates bursting-cols) ])
        ))) ]))
                                                                                            ;
(define (compute-learning-mode l2        ;; L2 {CellX} {CellX} {{CellX}} {CellX} ->
          feedforward-input context-input feedforward-growth-candidates bursting-cols)
  ;; in learning mode, maintain prior activity or create random sdr for new object
  (define (new-sdr)                      ;; -> SDR
    ;; produce random sdr with sdr-size bits; create proximal segments
    (when (and (fxzero? (tm-cortical-column l2)) (fxzero? (l2-n-sdrs l2)))
      (random-seed (l2-seed l2)))        ;; first sdr of first column is deterministic
    (l2-n-sdrs-set! l2 (fx1+ (l2-n-sdrs l2)))
    (let* ( [sample (u32-sample (iota (l2-cell-count l2)) (l2-sdr-size l2))]
            [sdr    (sort-unique! sample (l2-sdr-size l2))])
      #;
      (at-new-segment-cells (pba-axon-tree (tm-proximal-pba l2)) sdr)
      sdr))
  ;; (compute-learning-mode)
  (let ([prev-active-cells (get-active-cells l2)])
    (when (cond
        [ (fx<? (length prev-active-cells) (l2-min-sdr-size l2))
            ;; not enough previously active cells: create a new sdr and learn on it
            (tm-active-cells-set! l2 (new-sdr))
            #t ]
        [ (fx>? (length prev-active-cells) (l2-max-sdr-size l2))
            ;; union of cells active: don't learn
            #f ]
        [ else #t ] )
      ;; do the actual learning

#|
; data analysis: cp                        tm-pyramidal-layer
; - permanences[active-cells]              active/matching segments
;                                          segments to punish
; - active cells [sources in layer]
; - feedforward cells [any sources]        reinforce-candidates
; - ff growth candidates [any sources]     growth-candidates
; - parameters
|#

#|
depolarize-proximal:

(learn cp (cp-proximal-permanences cp)
  (cp-active-cells cp) feedforward-input feedforward-growth-candidates
  (cp-sample-size-proximal cp) (cp-initial-proximal-permanence cp)
  (cp-syn-perm-proximal-inc cp) (cp-syn-perm-proximal-dec cp))
(define (learn cp permanences            ;; CP Permanences {CellX} {Source} {Source} Nat Perm Perm Perm ->
          active-cells active-input growth-candidate-input
          sample-size initial-permanence permanence-increment permanence-decrement)
  ;; for each active cell, reinforce active synapses, punish inactive synapses,
  ;; and grow new synapses to input bits that the cell isn't already connected to
  (let ([active-input-vec (list->u32-vector active-input)])
    (adapt-synapses permanences active-cells active-input-vec
        permanence-increment permanence-decrement)
    (grow-synapses cp permanences active-cells active-input-vec growth-candidate-input
        (if (fxnegative? sample-size)  (greatest-fixnum)  sample-size )
        initial-permanence)))
(define (adapt-synapses permanences      ;; Permanences {CellX} {Source} Perm Perm ->
          active-cells active-input-vec permanence-increment permanence-decrement)
  ;; update synapses: strengthen those connected to input, weaken others, remove on zero
  (for-each (lambda (cellx)
      (synapses-update! (lambda (synapse)
          (let* ( [source  (syn-source synapse)]
                  [perm    (if (u32-search active-input-vec source)
                             (clip-max (fx+ (syn-perm synapse) permanence-increment))
                             (fx- (syn-perm synapse) permanence-decrement))])
            (and (fxpositive? perm) (make-syn source perm))))
        (vector-ref permanences cellx)))
    active-cells))
|#        

      (tm-activate-cells l2
          feedforward-input              ;; proximal-input
          feedforward-input              ;; proximal-reinforce-candidates
          context-input                  ;; basal-reinforce-candidates
          '()                            ;; apical-reinforce-candidates
          feedforward-growth-candidates  ;; proximal-growth-candidates
          context-input                  ;; basal-growth-candidates
          '()                            ;; apical-growth-candidates
          #t                             ;; learning
          bursting-cols #;'())                           ;; no bursting in L2
      #;
      (when (pair? lateral-inputs)
        (for-each (lambda (lateral-input distal-permanence)  ;; external distal learning
            (learn l2 distal-permanence
              (l2-active-cells l2) lateral-input lateral-input
              (l2-sample-size-distal l2) (l2-initial-distal-permanence l2)
              (l2-syn-perm-distal-inc l2) (l2-syn-perm-distal-dec l2)))
          lateral-inputs (l2-distal-permanences l2)))
      #;
      (learn l2 (l2-internal-distal-permanences l2)
        (l2-active-cells l2) distal-input distal-input
        (l2-sample-size-distal l2) (l2-initial-distal-permanence l2)
        (l2-syn-perm-distal-inc l2) (l2-syn-perm-distal-dec l2))
      )))
                                                                                            ;
(define (compute-inference-mode l2       ;; L2 {CellX} {CellX} ->
          feedforward-input distal-input)
  ;; in inference mode, if there is some feedforward activity, recognize previously
  ;; known objects and activate a subset of the cells with feedforward support;
  ;; otherwise use lateral activity to activate a subset of the previous active cells
  #f)

|#                                                                                            ;

(define (l2-print-statistics l2s)           ;; (Vector CCX->L2)
  ;; accumulate L2 counts
  (define (sum-l2 f)                     ;; (CP -> Nat) -> Nat
    (vector-fold-left (lambda (sum l2)
        (+ sum (f l2)))
      0
      l2s))
  (when (positive? (sum-l2 cp-n-sdrs))
    (for-each display `(
        ,(sum-l2 cp-n-sdrs) " sdrs created\n"
        "syns/segments   " ,(sum-l2 number-of-proximal-synapses) "/"
                           ,(sum-l2 number-of-proximal-segments) " p23 proximal\n"))
    (when (positive? (sum-l2 number-of-distal-synapses))
      (for-each display `(
          "                " ,(sum-l2 number-of-distal-synapses) "/"
                             ,(sum-l2 number-of-distal-segments) " p23 distal\n")))
    (when (positive? (sum-l2 number-of-lateral-synapses))
      (for-each display `(
          "                " ,(sum-l2 number-of-lateral-synapses) "/"
                             ,(sum-l2 number-of-lateral-segments) " p23 lateral\n")))))

#;(tests for pyramidal-temporal-memory as column pooler algorithm)
#|
(define (set l)                          ;; {Fixnum} -> {Fixnum}
  ;; produce standard sdr representation
  (sort-unique! l))
                                                                                            ;
(define (range start . limit)            ;; Nat [(Nat)] -> {Nat}
  ;; produce list of values from start to (limit-1)
  (if (null? limit)  (range 0 start)
    (build-list (fx- (car limit) start)
      (lambda (i) (+ start i)))))
                                                                                            ;
(define (test name thunk)                ;; String ( -> ) ->
  ;; apply thunk (which contains expects), display name and number passed
  (display name)
  (thunk)
  (for-each display `(": " ,(expects) " tests passed\n")))
                                                                                            ;
(define initialize-default-pooler (case-lambda [(kwargs)
  ;; Initialize and return a default ColumnPooler
  (make-l2 (append `([input-width      . ,(* 2048 8)]
                     [cell-count       . 2048]
                     [column-count     . 128])  ;; override for tests
               kwargs)) ]
  [() (initialize-default-pooler '()) ]))
                                                                                            ;
(expects)                              ;; clear test count
                                                                                            ;
#;(replace "pooler" in tests by "l2")
(test "Constructor" (lambda ()                 ;; Create a simple instance and test the constructor
  (let ([l2 (initialize-default-pooler)])
    (expect [(number-of-cells   l2)  2048]
            [(number-of-inputs  l2)  16384]
            [(number-of-proximal-synapses 
                    l2 (range 2048))  0]
            [(num-connected-proximal-synapses 
                    l2 (range 2048))  0])) ))
                                                                                            ;
#;
(test "InitialNullInputLearnMode" (lambda ()   ;; Tests with no input in the beginning
  (let ([l2 (initialize-default-pooler)])
    ;; Should be no active cells in beginning
    (expect [(length (get-active-cells l2))  0])
    ;; After computing with no input should have active cells
    (l2-compute l2 (list) #t)
    (let ([object-sdr1 (set (get-active-cells l2))])
      (expect [(length object-sdr1)  40])
      ;; Should be no active cells after reset
      (l2-reset l2)
      (expect [(length (get-active-cells l2))  0])
      ;; Computing again with no input should lead to different active cells
      (l2-compute l2 (list) #t)
      (let ([object-sdr2 (set (get-active-cells l2))])
        (expect [(length object-sdr2)  40]
                [(< (length (intersect1d object-sdr1 object-sdr2)) 5) #t ]))))))
                                                                                            ;
(test "InitialProximalLearning" (lambda ()     ;; Tests the first few steps of proximal learning
  (let ([l2 (initialize-default-pooler)])
    ;; Get initial activity
    (l2-compute l2 (range 0 40) #t (range 0 40))
    (expect [(length (get-active-cells l2))  40])
        (let ([object-sdr (set (get-active-cells l2))])
      ;; Ensure we've added correct number synapses on the active cells
      (expect [(number-of-proximal-synapses l2 (get-active-cells l2))
               (* 40 20)])
      ;; Ensure they are all connected
      (expect [(num-connected-proximal-synapses l2 (get-active-cells l2))
               (* 40 20)])
      ;; As multiple different feedforward inputs come in, the same set of cells should be active
      (l2-compute l2 (range 100 140) #t)
      (expect [(set (get-active-cells l2)) object-sdr])
      (expect [(num-connected-proximal-synapses l2 (get-active-cells l2))
               (* 40 40)])
      ;; If there is no feedforward input we should still get the same set of active cells
      (l2-compute l2 (list) #t)
      (expect [(set (get-active-cells l2)) object-sdr])
      ;; In "learn new object mode", given a familiar feedforward input after reset we should not get the same set of active cells
      (l2-reset l2)
      (l2-compute l2 (range 0 40) #t)
      (expect [(equal? object-sdr (set (get-active-cells l2))) #f ])
      (expect [(length (get-active-cells l2)) 40]))) ))
                                                                                           ;


(test "InitialInference" (lambda ()            ;; Tests inference after learning one pattern
  (let ([pooler (initialize-default-pooler)])
    (cp:compute pooler (range 0 40) #t)      ;; Learn one pattern
    (let ([object-sdr (set (cp:get-active-cells pooler))])
      (cp:compute pooler (range 0 40) #t)    ;; Form internal distal connections
      (l2-reset pooler)
      (cp:compute pooler (range 0 40) #f)    ;; Inferring on same pattern should lead to same result
      (expect [(set (cp:get-active-cells pooler)) object-sdr])
      (cp:compute pooler (list) #f)          ;; Inferring with no inputs should maintain same pattern
      (expect [(set (cp:get-active-cells pooler)) object-sdr]))) ))
                                                                                            ;
(test "ShortInferenceSequence" (lambda ()      ;; Tests inference after learning two objects with two patterns
  (let ([pooler (initialize-default-pooler)])
    (cp:compute pooler (range 0 40) #t)
    (let ([object1-sdr (set (cp:get-active-cells pooler))])
      (cp:compute pooler (range 100 140) #t)
      (expect [(set (cp:get-active-cells pooler)) object1-sdr])
      (l2-reset pooler)
      (cp:compute pooler (range 1000 1040) #t)
      (let ((object2-sdr (set (cp:get-active-cells pooler))))
        (cp:compute pooler (range 1100 1140) #t)
        (expect [(set (cp:get-active-cells pooler)) object2-sdr])
        (l2-reset pooler)
        (cp:compute pooler (range 100 140) #f)
        (expect [(set (cp:get-active-cells pooler)) object1-sdr])
        (cp:compute pooler (list) #f)
        (expect [(set (cp:get-active-cells pooler)) object1-sdr])
        (l2-reset pooler)
        (cp:compute pooler (range 0 40) #f)
        (expect [(set (cp:get-active-cells pooler)) object1-sdr])
        (l2-reset pooler)
        (cp:compute pooler (range 1100 1140) #f)
        (expect [(set (cp:get-active-cells pooler)) object2-sdr])
        (cp:compute pooler (list) #f)
        (expect [(set (cp:get-active-cells pooler)) object2-sdr])
        (l2-reset pooler)
        (cp:compute pooler (range 1000 1040) #f)
        (expect [(set (cp:get-active-cells pooler)) object2-sdr])))) ))
                                                                                            ;
(test "ProximalLearning_SampleSize" (lambda ()
  ;; During learning, cells should attempt to have sampleSizeProximal active proximal synapses
  (let ([pooler (initialize-default-pooler `(
                  [initial-proximal-permanence   . ,(perm 0.60)]
                  [connected-proximal-permanence . ,(perm 0.50)]
                  [sample-size-proximal          . 10]
                  [syn-perm-proximal-dec         . 0]))]
        [feedforward-input-1 (range 10)])
    (cp:compute pooler feedforward-input-1 #t)
    (for-each (lambda (cell)
        (expect [(cp:number-of-proximal-synapses pooler (list cell)) 10]
                [(cp:num-connected-proximal-synapses pooler (list cell)) 10])
        (let* ( 
            [segment (vector-ref (cp:cp-proximal-permanences pooler) cell)]
            [presynaptic-cells (synapses-map syn-source segment)]
            [permanences       (synapses-map syn-perm segment)])
          (expect [(set presynaptic-cells) (set feedforward-input-1)])
          (for-each (lambda (p)
              (expect [p (perm 0.60)]))
            permanences)))
      (cp:get-active-cells pooler))
    (cp:compute pooler (range 10 20) #t)
    (for-each (lambda (cell)  ;; "Should connect to every active input bit."
        (expect [(cp:number-of-proximal-synapses pooler (list cell)) 20]))
      (cp:get-active-cells pooler))
    (cp:compute pooler (range 15 25) #t)
    (for-each (lambda (cell)  ;; "Should connect to every active input bit that it's not yet connected to."
        (expect [(cp:number-of-proximal-synapses pooler (list cell)) 25]))
      (cp:get-active-cells pooler))
    (cp:compute pooler (range 0 30) #t)
    (for-each (lambda (cell)  ;; "Should not grow more synapses if it had lots active."
        (expect [(cp:number-of-proximal-synapses pooler (list cell)) 25]))
      (cp:get-active-cells pooler))
    (cp:compute pooler (range 23 30) #t)
    (for-each (lambda (cell)  ;; "Should grow as many as it can/all connected."
        (expect [(cp:number-of-proximal-synapses pooler (list cell)) 30])
        (expect [(cp:num-connected-proximal-synapses pooler (list cell)) 30]))
      (cp:get-active-cells pooler))
    )))                                                                               ;

|#

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
