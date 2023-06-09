;; github.com/rogerturner/.../layer-4.ss © 2021 Roger Turner
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; see *Notices* below for License and Contact links

#| HTM-scheme layer-4: 

Models layer 4 of a cortical column with inputs:
- feedforward: tcs (thalamo-cortical "feature")
- context:     p6l4 (? "location")
- feedback:    to p4 apical dendrites
- ss4l4, ss4l23, p4  recurrent activity 

  Layer4 consists of ss4(L4), ss4(L23), and p4 populations, each modelled with
  tm-pyramid-layer (a variant of the Numenta apical_tiebreak_temporal_memory algorithm).

  The /main/ connections are shown below

                 ^      ^       v
                 |      |       |
                 ^      ^       v
              +--ac-----pc-+-+--ai--+
        +---->bi |  p4  |  |*|      |
        |     +--|------|--|*|------+               ac = active cells
        |        ^      ^  |*|                      (output is combination
        |     +--ac-----pc-+*+------+                of p4 and ss4L23)
        +---->bi  ss4(L23) |*|      |
        |     +------------|*|------+               ai = apical input (from p2/3)
        |                  |*|
        |  +-----+         |*|                      bi = basal input
        |  |     ^         |*|
        |  |  +--ac--------|*|------+               pc = predicted cells
        |  +->bi  ss4(L4)  |*|      |
        |     +------------+-+------+
        |                   ^ \
        |                   |  \ minicolumn: any depolarized active cell inhibits others
        ^                   ^ 
   p6(L4) "location"    tcs "feature"
        
    Macrocolumn layers of pyramidal (and spiny stellates in L4, which are treated as
    pyramids without apical dendrites) are updated in 5 stages:
    - active presynaptic cells with basal/apical (context/feedback) connections
      -> active/matching segments [uses active-cols to optimize active segs calc]
    - active basal/apical segments -> predicted cells
    - active presynaptic cells with proximal (feedforward) connections, predicted cells
      -> inhibited cells (minicolumn for L4, neighbouring minicols for L2?)
    - active presynaptic cells with proximal connections, inhibited cells
      -> active postsynaptic cells
    - active postsynaptic cells, active/matching segments
      -> adjust/add synapses on active segments, add segments

    Connections are based on
    [Izhikevich & Edelman 2008 "Large-scale model of mammalian thalamocortical systems"]
    (10.1073/pnas.0712231105) figure 2, and figures 8 and 9 in Supporting Information.
    "basket interneurons in L4, L5, and L6 have axons confined within a single cortical layer,
    whereas basket cells in L2/3 and non-basket interneurons may have axons spanning several layers"
    "Basket cells have fast spiking (FS) firing patterns [2]"
    [Lundqvist etal 2010]

  |#

  #!chezscheme

(library (frameworks layer-4)
                                                                                            ;
(export
make-l4
l4-ss4l4
l4-ss4l23
l4-p4
l4-depolarize-cells
l4-inhibit-cells
l4-activate-cells
l4-reset
l4-reset-seq
l4-active-cells
l4-learning-cells
l4-predicted-cells
l4-predicted-active-cells
l4-bursting-cols
l4-print-statistics
  )
                                                                                            ;
(import
  (chezscheme)
  (parameters)
  (frameworks htm-prelude)
  (frameworks coordinates)
  (frameworks htm-concept)
  (algorithms tm-pyramid-layer))
                                                                                            ;
  (implicit-exports #f)

;; See htm-concept.ss for type and data structure description and code conventions
  
  ;; source layer coding:
  ;;    0  "location" (p6l4?)
  ;;    1  apical-input (p23)
  ;;    2  ss4l4
  ;;    3  ss4l23
  ;;    4  p4
  
;; === Layer record ===
                                                                                            ;
(define-record-type l4 (fields           ;; L4
    ss4l4                                ;; ATTM [ss4, axon -> L4]
    ss4l23                               ;; ATTM [ss4, axon -> L2/3]
    p4                                   ;; ATTM [p4, with apical dendrite]
    (mutable inhibited-cols)             ;; {ColX}
    (mutable bursting-cols))
(protocol #;(make-l4)
  ;; make attm instances: each L4 population configured with:
  ;;   feedforward "feature" input to all cells in minicolumn
  ;;   filter proc(s) identifying sub-layer (ss4l4/ss4l23/p4)
  (lambda (new)
    (lambda (
        ccx                              ;; index of this cortical column
        ss4l4-c/mc                       ;; cells/minicolumn for ss4l4 cells
        ss4l23-c/mc                      ;; ..ss4l23
        p4-c/mc                          ;; ..p4
        filter-ss
        tm-parameters)
      (define (make-proximal-at c/mc)    ;; Nat -> AxonTree
        ;; initialisation for L4 with thalamic input to all cells in mc layer
        ;; proximal input to L4 is *minicolumn* SDR
        ;; for each minicolumn in layer:
        ;;   for each cell in minicolumn:
        ;;     create AxonTree entry with Input->Target->Segment, where
        ;;       Input is minicolumn index and Segment has a connected Synapse
        (let ([at (make-at *mc/cc* ccx)])
          (do ([colx 0 (fx1+ colx)]) ((fx=? colx *mc/cc*) at )
            (let* ( [first (fx* c/mc colx)]
                    [limit (fx+ first c/mc)]
                    [ss    (list (make-source 0 0 colx))])
              (do ([cellx first (fx1+ cellx)]) ((fx=? cellx limit))
                (let ([seg (at-make-seg at colx)])
                  (at-update at colx ss (perm 1.0) seg #f)))))))
                                                                                            ;
      (define (proximal->cols at proximal-input)  ;; AxonTree {InputX} -> {ColX}
        ;; produce cells to be activated by proximal-input
        (let next-source ([sources proximal-input] [cellxs (list)])
          (if (pair? sources)
            (let* ( [target     (at-target at (car sources))]
                    [segxx-last (segxv-last target)])
              (let next-segment ([segxx segxv-base] [cellxs cellxs])
                (if (fx<=? segxx segxx-last)
                  (let ([segment (at-seg-ref at (segxv-ref target segxx))])
                    (next-segment (fx+ segxx segx-bytes)
                                  ;; seg-cellx is actually InputX, ie colx
                                  (cons (seg-cellx segment) cellxs)))
                  (next-source (cdr sources) cellxs))))
            cellxs)))
                                                                                            ;
;; (make-l4)
    (let ([dimensions `([column-count      . ,*mc/cc*]
                        [cortical-column   . ,ccx])])
      (new
        (make-tm (append tm-parameters
            `([cells-per-column     . ,ss4l4-c/mc]
              [layer                . 2]
              [basal-filter         . ,(lambda (sources post-mcx)
                                         (filter-ss sources post-mcx ccx 'ss4l4))]
              [make-proximal-at     . ,(lambda () (make-proximal-at ss4l4-c/mc))]
              [proximal->cols       . ,proximal->cols])
            dimensions))
        (make-tm (append tm-parameters
            `([cells-per-column     . ,ss4l23-c/mc]
              [layer                . 3]
              [basal-filter         . ,(lambda (sources post-mcx)
                                         (filter-ss sources post-mcx ccx 'ss4l23))]
              [make-proximal-at     . ,(lambda () (make-proximal-at ss4l23-c/mc))]
              [proximal->cols       . ,proximal->cols])
            dimensions))
        (make-tm (append tm-parameters
            `([cells-per-column     . ,p4-c/mc]
              [layer                . 4]
              [basal-filter         . ,(lambda (sources post-mcx)
                                         (filter-ss sources post-mcx ccx 'p4b))]
              [apical-filter        . ,(lambda (sources post-mcx)
                                         (filter-ss sources post-mcx ccx 'p4a))]
              [make-proximal-at     . ,(lambda () (make-proximal-at p4-c/mc))]
              [proximal->cols       . ,proximal->cols])
            dimensions))
        (list) (list)))))))

;; === Layer algorithm ===
                                                                                            ;

(define (l4-depolarize-cells l4          ;; L4 {ColX} {Source} {Source} {Source} Boolean ->
          feedforward-input              ;; thalamo-cortical driving input "feature"
          location-input                 ;; (for this and neighbouring ccs)
          context-input                  ;; previous l4
          feedback-input                 ;; previous p23
          learn)
  ;; Depolarize each population based on current location input and previous layer activity
  (let ([basal-input (union1d location-input context-input)])
    (tm-depolarize-cells (l4-ss4l4  l4) feedforward-input basal-input '() learn)
    (tm-depolarize-cells (l4-ss4l23 l4) feedforward-input basal-input '() learn)
    (tm-depolarize-cells (l4-p4     l4) feedforward-input basal-input feedback-input learn)))
                                                                                            ;

(define (l4-inhibit-cells l feedforward-input) ;; L4 {ColX} ->
  ;; Calculate columns inhibited by early firing of predicted cells
  ;; Used in activate-cells to define bursting-columns
  (define (inhibited-cols tm)            ;; TM -> {ColX}
    ;; produce cols with predicted cells with ff input
    (intersect1d (map cellx->colx (tm-predicted-cells tm)) feedforward-input))
  (l4-inhibited-cols-set! l (union1d
      (inhibited-cols (l4-ss4l4 l)) (inhibited-cols (l4-ss4l23 l)) (inhibited-cols (l4-p4 l)))))
                                                                                            ;
(define (neighbours colx)                ;; ColX -> {ColX}
  ;; produce adjacent cols (including colx)
  (vector-ref neighbours-list colx))
                                                                                            ;
#;
(define (l4-inhibit-cells l feedforward-input) ;; L4 {ColX} ->
  ;; Calculate columns inhibited by early firing of predicted cells
  ;; Used in activate-cells to define bursting-columns
  (define (inhibited-cols tm)            ;; TM -> {ColX}
    ;; produce cols adjacent to cols with predicted cell with ff input
    (sort-unique!
      (flat-map neighbours
        (intersect1d (map cellx->colx (tm-predicted-cells tm)) feedforward-input))))
  (l4-inhibited-cols-set! l (union1d
      (inhibited-cols (l4-ss4l4 l)) (inhibited-cols (l4-ss4l23 l)) (inhibited-cols (l4-p4 l)))))
                                                                                            ;
(define (l4-activate-cells l             ;; L4 {Source} {Source} {Source} {Source} {Source} {Source} Boolean ->
          feedforward-input              ;; thalamo-cortical driving input "feature"
          location-input                 ;; context input (l6p4?)
          context-input                  ;; previous l4
          l4-learning                    ;; previous l4
          feedback-input                 ;; previous p23 + ss4l23 + p4
          l4-apical-learning             ;; ss4l23 + p4
          learn)
  ;; Activate and learn based on all active/learning cells connected to this cc and its inhibited cols
  (let (
      [basal-input       (union1d location-input context-input)]
      [basal-growth      (union1d location-input l4-learning)]
      [bursting-columns  (setdiff1d feedforward-input (l4-inhibited-cols l))])
    (tm-activate-cells (l4-ss4l4 l)
      feedforward-input            ;; proximal-input
      feedforward-input            ;; proximal-reinforce-candidates
      basal-input                  ;; basal-reinforce-candidates
      '()                          ;; apical-reinforce-candidates
      feedforward-input            ;; proximal-growth-candidates
      basal-growth                 ;; basal-growth-candidates
      '()                          ;; apical-growth-candidates
      learn 
      bursting-columns)
    (tm-activate-cells (l4-ss4l23 l)
      feedforward-input            ;; proximal-input
      feedforward-input            ;; proximal-reinforce-candidates
      basal-input                  ;; basal-reinforce-candidates
      '()                          ;; apical-reinforce-candidates
      feedforward-input            ;; proximal-growth-candidates
      basal-growth                 ;; basal-growth-candidates
      '()                          ;; apical-growth-candidates
      learn 
      bursting-columns)
    (tm-activate-cells (l4-p4 l)
      feedforward-input            ;; proximal-input
      feedforward-input            ;; proximal-reinforce-candidates
      basal-input                  ;; basal-reinforce-candidates
      feedback-input               ;; apical-reinforce-candidates
      feedforward-input            ;; proximal-growth-candidates
      basal-growth                 ;; basal-growth-candidates
      l4-apical-learning           ;; apical-growth-candidates
      learn 
      bursting-columns)
    (l4-bursting-cols-set! l bursting-columns)))
                                                                                            ;
(define (l4-reset l)                        ;; L4 ->
  (tm-reset (l4-ss4l4 l))
  (tm-reset (l4-ss4l23 l))
  (tm-reset (l4-p4 l)))
                                                                                            ;
(define (l4-reset-seq l)                    ;; L4 ->
  (tm-reset (l4-ss4l4 l)))

;; === Accessors ===
                                                                                            ;
(define (get f l pop)                    ;; (ATTM -> X) L4 L4Pop -> X
  ;; access f cells of sub-population pop of layer l
  (case pop
    [ (ss4l4)  (f (l4-ss4l4  l)) ]
    [ (ss4l23) (f (l4-ss4l23 l)) ]
    [ (p4)     (f (l4-p4     l)) ] ))
                                                                                            ;
(define (getx f l pop x)                 ;; (ATTM -> X) L4 L4Pop -> X
  ;; access f cells of sub-population pop of layer l
  (case pop
    [ (ss4l4)  (f (l4-ss4l4  l) x) ]
    [ (ss4l23) (f (l4-ss4l23 l) x) ]
    [ (p4)     (f (l4-p4     l) x) ] ))
                                                                                            ;
(define (l4-active-cells l pop)         ;; L4 L4Pop -> {CellX}
  (get tm-active-cells l pop))
                                                                                            ;
(define (l4-learning-cells l pop)       ;; L4 L4Pop -> {CellX}
  (get tm-learning-cells l pop))
                                                                                            ;
(define (l4-predicted-cells l pop)      ;; L4 L4Pop -> {CellX}
  (get tm-predicted-cells l pop))
                                                                                            ;
(define (l4-predicted-active-cells l p) ;; L4 L4Pop -> {CellX}
  (get tm-predicted-active-cells l p))

;; === Statistics ===
                                                                                            ;
(define (l4-number-of-connected-cells l p ) ;; L4 L4Pop -> Nat
  (get number-of-connected-cells l p))
                                                                                            ;
(define (l4-number-of-proximal-synapses l pop) ;; L4 L4Pop -> Nat
  (get number-of-proximal-synapses l pop))
                                                                                            ;
(define (l4-number-of-basal-synapses l pop) ;; L4 L4Pop -> Nat
  (get number-of-basal-synapses l pop))
                                                                                            ;
(define (l4-number-of-apical-synapses l p)  ;; L4 L4Pop -> Nat
  (get number-of-apical-synapses l p))
                                                                                            ;
(define (l4-number-of-proximal-segments l pop) ;; L4 L4Pop -> Nat
  (get number-of-proximal-segments l pop))
                                                                                            ;
(define (l4-number-of-basal-segments l pop) ;; L4 L4Pop -> Nat
  (get number-of-basal-segments l pop))
                                                                                            ;
(define (l4-number-of-apical-segments l p)  ;; L4 L4Pop -> Nat
  (get number-of-apical-segments l p))
                                                                                            ;
(define (l4-connection-lengths l p x)
  (getx connection-lengths l p x))
                                                                                            ;
(define (l4-print-statistics l4s)           ;; (Vector CCX->L4)
  (define (sum-l4 f pop)
    (vector-fold-left (lambda (sum layer)
        (let ([lay->n (f layer pop)])
          (if (vector? lay->n)
            (vector-map +
              (if (vector? sum)  sum
                  (make-vector (vector-length lay->n) 0))
              lay->n)
            (+ sum lay->n))))
      0
      l4s))
  ;(print-axon-arbor l4s)
  (let ([label "syns/segs/cells "])
    (define (print-pop pop n-syns n-segs kind)
      (let ([ncc (sum-l4 l4-number-of-connected-cells pop)])
        (when (positive? ncc)
          (for-each display `( ,label
              ,(sum-l4 n-syns pop) "/"
              ,(sum-l4 n-segs pop) "/"
              ,ncc " " ,(symbol->string pop) " " ,kind "\n"))
          (set! label "                "))))
    (print-pop 'ss4l4 l4-number-of-proximal-synapses l4-number-of-proximal-segments "proximal")
    (print-pop 'ss4l4 l4-number-of-basal-synapses l4-number-of-basal-segments "basal")
    (print-pop 'ss4l23 l4-number-of-proximal-synapses l4-number-of-proximal-segments "proximal")
    (print-pop 'ss4l23 l4-number-of-basal-synapses l4-number-of-basal-segments "basal")
    (print-pop 'p4 l4-number-of-proximal-synapses l4-number-of-proximal-segments "proximal")
    (print-pop 'p4 l4-number-of-basal-synapses l4-number-of-basal-segments "basal")
    (print-pop 'p4 l4-number-of-apical-synapses l4-number-of-apical-segments "apical"))
  #;
  (let ([label "pre-index (avg) "])
    (define (mean-l4 pop kind)
      (vector-fold-left (lambda (acc l4)
          (call-with-values
            (lambda () (l4-connection-lengths l4 pop kind))
            (lambda (total count)
              (cons (+ (car acc) total) (+ (cdr acc) count)))))
        (cons 0 0)
        l4s))
    (define (print-pre-index pop kind)
      (let ([counts (mean-l4 pop kind)])
        (when (positive? (cdr counts))
          (for-each display `( ,label
              ,(/ (inexact (quotient (* 10 (car counts)) (cdr counts))) 10)
              " " ,(symbol->string pop) " " ,(symbol->string kind) "\n"))
          (set! label "                "))))
    (print-pre-index 'ss4l4  'proximal)
    (print-pre-index 'ss4l4  'basal)
    (print-pre-index 'ss4l23 'proximal)
    (print-pre-index 'ss4l23 'basal)
    (print-pre-index 'p4     'proximal)
    (print-pre-index 'p4     'basal)
    (print-pre-index 'p4     'apical)))
                                                                                            ;
(define (print-axon-arbor l4s)
  ;; print minicol lattice showing post-synaptic cols for a cell in cc0
  (define (segs->ccolxs tm ccx source segs)  ;; TM CCX Source {Seg} -> {Nat}
    ;; map segs for a source to ccx<+colx (unconnected => negative)
    (let ([connected (tm-connected-permanence tm)])
      (map (lambda (seg)
          (let ([synapse (synapses-search source seg)]
                [mcx (fx+ (fx* *mc/cc* ccx)
                          (cellx->colx (seg-cellx seg)))])
            (if synapse
              (if (fx>=? (syn-perm synapse) connected)  mcx
                  (fx- mcx))
              0)))
        segs)))
  (let ([ncc (vector-length l4s)])
    (when (and hex-mc-lattice (or (= ncc 7) (= ncc 19) (= ncc 37)))
      (let* (
      [l4-pop #;l4-ss4l4 #;l4-ss4l23  l4-p4]
       ;; only 7 / 19 / 37 ccs
      [all-from-cc0                      ;; {[Source . {CColX}]}
        ;; iterate over all ccs, l4 layers finding sources in cc0, accumulating targets
        ;; => list of pairs: [cell in cc0 . list of ccx<+colx of projected to segments]
        (vector-fold-left                ;; accumulate source l4-pop for all ccs
          (lambda (ccs-connections l4 ccx)
            ;; accumulate layers
            (define (tm->connections tm)    ;; TM -> {[Source . {CColX}]}
              ;; produce post colx from other ccs where source is p4 in cc0
              (if (fxzero? (number-of-cells tm))  '()
                  (let-values ([(sources segss) (get-axon-tree tm 'basal)])
                    (fold-left              ;; accumulate sources
                      (lambda (sources-connections source segs)
                        (if (and (fx=? 0 (source-ccx source)) (pair? segs))
                          (cons (cons source (segs->ccolxs tm ccx source segs)) sources-connections)
                          sources-connections))
                      (list)
                      sources segss))))
            (append (tm->connections (l4-pop l4)) ccs-connections))
          (list)
          (vector-take ncc l4s) (list->vector (iota ncc))) ]
      [all-from-cc0                      ;; {[Source . {CColX}]}
        (sort (lambda (p1 p2) (fx<? (car p1) (car p2))) all-from-cc0)]
      [condensed-from-cc0                ;; {[Source . {CColX}]}
        ;; for each source, combine projected to lists for ccs
        (let each-run ([sources all-from-cc0] [out (list)])
          (cond
            [(null? sources) out ]
            [else
              (let ([this-source (caar sources)])
                (let each-source ([sources sources] [ccolxs (list)])
                  (cond
                    [(or (null? sources) (not (fx=? this-source (caar sources))))
                       (each-run sources (cons (cons this-source ccolxs) out)) ]
                    [else (each-source (cdr sources) (append (cdar sources) ccolxs)) ]))) ])) ]
      [chosen-cell                       ;; [Source . {CColX}]
        ;; cell in cc0 with 99th percentile connections to other ccs
        (let ([sorted (sort (lambda (x y)
                          (< (length (cdr x)) (length (cdr y))))
                        condensed-from-cc0)])
          (list-ref sorted (int<- (* (length sorted) .99)))) ])
            
    (let ([radius (case *mc/cc*
                    [( 61)  4]
                    [( 91)  5]
                    [(127)  6]
                    [(169)  7]
                    [(217)  8]
                    [(271)  9]
                    [(331) 10]
                    [(397) 11])])
      (define (print height width)          
        (let ([canvas (build-vector height (lambda _ (make-string width #\ )))])
          (define (row ccx mcx)
            (+ (fxdiv height 2) (r-coord-of-cc-centre ccx) (r-coord-of-minicol mcx)))
          (define (col ccx mcx)
            (+ (fxdiv width 2) (r-coord-of-cc-centre ccx) (r-coord-of-minicol mcx)
                  (* 2 (q-coord-of-cc-centre ccx)) (* 2 (q-coord-of-minicol mcx))))
          (define (put-char ccx mcx ch)
            (string-set!
              (vector-ref canvas (row ccx mcx))
              (col ccx mcx) ch))
          (define (put-right ccx mcx ch)
            (string-set!
              (vector-ref canvas (row ccx mcx))
              (fx1+ (col ccx mcx)) ch))
          (define (put-left ccx mcx ch)
            (string-set!
              (vector-ref canvas (row ccx mcx))
              (fx1- (col ccx mcx)) ch))
          (define (put-above ccx mcx ch)
            (string-set!
              (vector-ref canvas (fx1- (row ccx mcx)))
              (fx1+ (col ccx mcx)) ch))
          (let* ( [mm-1 (fx- *mc/cc* radius)]
                  [mm-2 (fx- mm-1 radius)]
                  [mm-6 (fx1+ (fx- *mc/cc* (fx* 6 radius)))]
                  [mm-5 (fx+ radius mm-6)])
            (do ([ccx 0 (+ ccx 1)]) ((= ccx ncc))  ;; put minicolumns and cc boundaries
              (do ([mcx 0 (+ mcx 1)]) ((= mcx *mc/cc*))
                (put-char ccx mcx #\x22C5 ))       ;; minicolumn lattice
              (do ([mcx mm-1 (+ mcx 1)]) ((= mcx *mc/cc*))
                (put-right ccx mcx #\/ ))          ;; lower right
              (do ([mcx mm-2 (+ mcx 1)]) ((= mcx mm-1))
                (put-right ccx mcx #\_ ))          ;; bottom
              (do ([mcx mm-6 (+ mcx 1)]) ((= mcx mm-5))
                (put-right ccx mcx #\\ )))         ;; upper right
            (do ([ccx (case ncc [7 2] [19 9] [37 22]) (+ ccx 1)])
                ((= ccx (case ncc [7 5] [19 14] [37 29])))
              (do ([mcx mm-5 (+ mcx 1)]) ((= mcx (+ mm-5 radius)))
                (put-above ccx mcx #\_ )))         ;; top edge
            (do ([ccx (case ncc [7 3] [19 11] [37 25]) (+ ccx 1)])
                ((= ccx (case ncc [7 6] [19 16] [37 32])))
              (do ([mcx (fx1- (+ mm-5 radius)) (+ mcx 1)]) ((= mcx (fx1- (+ mm-5 radius radius))))
                (put-left ccx mcx #\/ )))          ;; upper left edge
            (do ([ccx (case ncc [7 4] [19 13] [37 28]) (+ ccx 1)])
                ((= ccx (case ncc [7 7] [19 18] [37 35])))
              (do ([mcx (+ mm-5 radius radius) (+ mcx 1)]) ((= mcx (+ mm-5 radius radius radius)))
                (put-left ccx mcx #\\ ))))         ;; lower left edge
          (for-each (lambda (ccolx)
              (let* (
                  [not-connected (fxnegative? ccolx)]
                  [ccolx (fxabs ccolx)]
                  [ccx  (fxdiv ccolx *mc/cc*)]
                  [mcx  (fxmod ccolx *mc/cc*)]
                  [ch   (string-ref (vector-ref canvas (row ccx mcx)) (col ccx mcx))])
                (if (and not-connected (char=? ch #\x22C5))
                  (put-char ccx mcx #\x25CF)
                  (put-char ccx mcx
                    (case ch
                      [(#\x22C5 #\x25CF)  #\x2776 ]  ;; 1
                      [(#\x2776)  #\x2777 ]
                      [(#\x2777)  #\x2778 ]
                      [(#\x2778)  #\x2779 ]
                      [(#\x2779)  #\x277A ]
                      [(#\x277A)  #\x277B ]
                      [(#\x277B)  #\x277C ]
                      [(#\x277C)  #\x277D ]
                      [(#\x277D)  #\x277E ]
                      [else       #\x277F ])))))
            (cdr chosen-cell))
          (put-char 0 (cellx->colx (source-cellx (car chosen-cell))) #\x25C9)
          (vector-for-each (lambda (s)
              (put-string (current-output-port) s) (newline))
            canvas)))
      (print (case ncc                   ;; height
              [(7)  (fx+ 4 (fx* radius 6))]
              [(19) (fx+ 6 (fx* radius 10))]
              [(37) 92])
             (case ncc                   ;; width
              [(7)  (fx- (fx* radius 12) 3)]
              [(19) (fx- (fx* radius 20) 10)]
              [(37) 145])))))))

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
