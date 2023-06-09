;; github.com/rogerturner/.../l2-l4-patch.ss © 2021 Roger Turner
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; see *Notices* below for License and Contact links

#| Scheme library, part of the untangling-sequences project.

See htm-concept.ss for type and data structure description and code conventions.
  
Models a patch of cortical columns with L2 and L4 layers:

  L2 is a "column pooler" layer, L4 is a "apical tiebreak temporal memory" layer
  (with ss4L4, ss4L23, and p4 cell populations). Connections used in Numenta code:

                            +---------------<- other L2 cc's active cells
                            v                    (from previous time step)
          +-----------------li--+             
          |          L2        id<--+--------> other L2 cc's lateral inputs  
          +--ff---gc--------ac--+   |            (for next time step)
             ^    ^         v       |              
             |    |         +-------+          ac = active cells
             ^    ^         v                  ai = apical input (to p4 only)
          +--ac---lc--------ai--+              bi = basal input
    +---->bi         L4         |              ff = feedforward input
    |     +-------ff------------+              gc = growth candidates  
    |             ^                            id = internal distal input
    |             |                            li = lateral input (per cc)
    ^             ^                            lc = learning cells
  location      feature

  |#
    
  #!chezscheme

(library (frameworks l2-l4-patch)
                                                                                            ;
(export
make-patch
  patch-l2s
  patch-l4s
compute
reset
  reset-l4
  reset-l2
  reset-seq
  print-statistics
  (rename
    (l2-active-cells           get-l2-active-cells)
    (l2-number-of-cells        get-l2-number-of-cells)
    (l4-active-cells           get-active-cells)
    (l4-predicted-cells        get-predicted-cells)
    (l4-predicted-active-cells get-predicted-active-cells)
    ))
                                                                                            ;
(import
  (except (chezscheme) reset)
          (parameters)
          (frameworks htm-prelude)
          (frameworks coordinates)
          (frameworks htm-concept)
          (frameworks layer-4)
          (frameworks layer-2))

  (implicit-exports #f)
  
#|
  pre-synaptic (axon source) layer coding: 0-4 p6l4/p23/ss4l4/ss4l23/p4
  post-synaptic (dendrite) coding for L4: 'ss4l4/'ss4l23/'p4b/'p4a
  [Izhikevich & Edelman 2008 Figure 8] gives axon spans of 1.0 1.12 .5 .15 (mm);
  using coordinate coding of 1 for minicol spacing of .05 mm, and squared distances
  => corresponding span values of 400 500 100 9 (see l2-span/l4-span below)
  (but [Schubert etal 2003,2007] => ss4 axons/dendrites are intra-cc only?)
  % pre->post connections [Izhikevich & Edelman 2008 Figure 9]:
            p6l4  p23  ss4l4  ss4l23  p4
  p23         2    60     1      7     8  (assume ss4l23+p4 -> p23 proximal, p23 -> distal)
  ss4l4      33     3    12      4     4  (% of ss4l4 synapses targeted by p6l4 etc)
  ss4l23     31     6    11      4     4
  p4b        31     4    12      4     4
  p4a         3    63     1      7     8     |#

;; === L2 L4 algorithm ===
                                                                                            ;
(define-record-type patch (fields        ;; Patch
  l2s                                    ;; (Vector CCX->L2)
  l4s                                    ;; (Vector CCX->L4)
  nearby-ccs                             ;; (Vector CCX->{CCX}) [ccs, incl self, that are near each cc]
  l2-counts                              ;; (Vector Kind->Nat)
  l4-counts                              ;; (Vector Kind->Nat)
  tp)                                    ;; ThreadPool
  (sealed #t) (opaque #t) (nongenerative patch)
(protocol
  (lambda (new)
    (lambda (ncc nmc l2-c/mc ss4l4-c/mc ss4l23-c/mc p4-c/mc l6-c/mc cp-overrides attm-overrides)
                                                                                            ;
;;          pre-kind:  p6l4   p23  ss4l4  ss4l23   p4  
(define l2-span (vector   9   500    36      64   500))

(define l4-span (vector 400     9   500     100     9))
#;
(define (l2-filter                       ;; Patch {Source} ColX CCX Boolean -> {Source}
          patch                          ;;
          sources                        ;; pre-synaptic active cells
          post-mcx                       ;; minicolumn within cc of post-synaptic cell
          post-ccx                       ;; cortical column of post-synaptic cell
          proximal)
  ;; filter sources for axon/dendrite intersection with this minicolumn
  (if #t #;(not hex-mc-lattice)
    ;; Numenta experiments: no topology, p23 or p4 -> p23, fixed sample size
    (filter (lambda (source)
        (let ([pre-kind (source-layer source)])
          (or (and (fx=? 4 pre-kind) proximal)
              (and (fx=? 1 pre-kind) (not proximal)))))
      sources)
    ;; hex lattice: use axon reach /and/ proportionate sampling of all sources
    (let ([pre->ss (make-vector 5 (list))])
      (define (candidate source pre-kind)  ;; Source Layer ->
      ;; filter sources that could reach post column, partition by pre-kind
      ;; for L2, post is either p23-proximal with pre ss4l23 or p4,
      ;; or p23-distal/lateral, with pre p23
        (let* (
            [pre-ccx    (source-ccx source)]
            [pre-mcx    (cellx->colx (source-cellx source))]
            [same-cc    (fx=? pre-ccx post-ccx)]
            [distance2  (if same-cc
                          (within-cc-distance2 pre-mcx post-mcx)
                          (mc-distance2 pre-ccx pre-mcx post-ccx post-mcx))]
            [radius     (vector-ref l2-span pre-kind)]
            [reach?     (or (fx<=? distance2 radius) #;
                            (fxzero? long-connect))])
          (when reach?
            (vector-set! pre->ss pre-kind (cons source (vector-ref pre->ss pre-kind))))))
      (define (sample-candidates)        ;; -> {Source}
        ;; sample sources to produce ratio (**) of feedforward/context connections
        (let ([l2-counts  (patch-l2-counts patch)])
          (let ([p23-count (vector-ref l2-counts 1)]
                [ss4l23-count (vector-ref l2-counts 3)]
                [p4-count  (vector-ref l2-counts 4)]
                [p23       (vector-ref pre->ss 1)]
                [ss4l23    (vector-ref pre->ss 3)]
                [p4        (vector-ref pre->ss 4)])
          (let ([all-pre-p23 (fx+ ss4l23-count p4-count p23-count)])
            (sort-unique! (cond
              [(fx=? 1 (vector-length (patch-l2s patch)))
                (append p23 ss4l23 p4) ]  ;; single cc: no lateral connections
              ;; sub-sample sources that are over quota
              [(fx>? (fx* 75 p23-count) (fx* 60 all-pre-p23))  ;; p23 synapses over quota
                (let ([use-p23 (fxdiv (length p23) 2)])
                  (vector-set! l2-counts 1 (fx+ use-p23 (vector-ref l2-counts 1)))
                  (vector-set! l2-counts 3 (fx+ (length ss4l23) (vector-ref l2-counts 3)))
                  (vector-set! l2-counts 4 (fx+ (length p4) (vector-ref l2-counts 4)))
                  (append (list-sample p23 use-p23) ss4l23 p4)) ]
              [(fx>? (fx* 75 ss4l23-count) (fx* 7 all-pre-p23))  ;; ss4l23 synapses over quota
                (let ([use-ss4l23 (fxdiv (length ss4l23) 2)])
                  (vector-set! l2-counts 1 (fx+ (length p23) (vector-ref l2-counts 1)))
                  (vector-set! l2-counts 3 (fx+ use-ss4l23 (vector-ref l2-counts 3)))
                  (vector-set! l2-counts 4 (fx+ (length p4) (vector-ref l2-counts 4)))
                  (append p23 (list-sample ss4l23 use-ss4l23) p4)) ]
              [(fx>? (fx* 75 p4-count) (fx* 8 all-pre-p23))  ;; p4 synapses over quota
                (let ([use-p4 (fxdiv (length p4) 2)])
                  (vector-set! l2-counts 1 (fx+ (length p23) (vector-ref l2-counts 1)))
                  (vector-set! l2-counts 3 (fx+ (length ss4l23) (vector-ref l2-counts 3)))
                  (vector-set! l2-counts 4 (fx+ use-p4 (vector-ref l2-counts 4)))
                  (append p23 ss4l23 (list-sample p4 use-p4))) ]
              [else
                  (vector-set! l2-counts 1 (fx+ (length p23) (vector-ref l2-counts 1)))
                  (vector-set! l2-counts 3 (fx+ (length ss4l23) (vector-ref l2-counts 3)))
                  (vector-set! l2-counts 4 (fx+ (length p4) (vector-ref l2-counts 4)))
                  (append p23 ss4l23 p4) ]))))))
      (if proximal  ;; tight loops to skip irrelevant candidates
        (let next-l23/p4? ([ss sources])
          (when (pair? ss)
            (let* ( [source   (car ss)]
                    [pre-kind (source-layer source)])
              (when (fx>? pre-kind 2) (candidate source pre-kind))
              (next-l23/p4? (cdr ss)))))
        (let next-p23? ([ss sources])
          (when (pair? ss)
            (let* ( [source   (car ss)]
                    [pre-kind (source-layer source)])
              (when (fx=? pre-kind 1) (candidate source pre-kind))
              (next-p23? (cdr ss))))))
      (sample-candidates))))
                  
(define (l2-filter                       ;; Patch {Source} ColX CCX Boolean -> {Source}
          patch                          ;;
          sources                        ;; pre-synaptic active cells
          post-mcx                       ;; minicolumn within cc of post-synaptic cell
          post-ccx                       ;; cortical column of post-synaptic cell
          post-kind)
  ;; filter sources for axon/dendrite intersection with this minicolumn
  (if #t #;(not hex-mc-lattice)
    ;; Numenta experiments: no topology, p23 or p4 -> p23, fixed sample size
    (filter (lambda (source)
        (let ([pre-kind (source-layer source)])
          (or (and (fx<? 2 pre-kind) (eq? post-kind 'proximal))
              (and (fx=? 1 pre-kind) (eq? post-kind 'distal)))))
      sources)
    ;; hex lattice: use axon reach /and/ proportionate sampling of all sources
(let ()
(define (sample-pre pre->ss pre-kind counts total %tot n-cand %pre)  ;; LayerX Nat Nat Nat -> {Source}
  ;; produce sources (maybe sub-sampled) of pre-kind
  (let* (
      [candidates (vector-ref pre->ss pre-kind)]
      [n-cand-pre (length candidates)]
      [total-pre  (vector-ref counts pre-kind)])
    (cond  ;; sub-sample if cc % over quota
           ;; (n-cand[pre]/n-cand > pre%/tot%) == n-cand[pre]*tot% > n-cand*pre%
      [(fx>? (fx* total-pre %tot) (fx* total %pre))
        (let ([sample (fxdiv n-cand-pre 5)])
          (vector-set! counts pre-kind (fx+ total-pre sample))
          (list-sample candidates sample)) ]
      [else
        (vector-set! counts pre-kind (fx+ total-pre n-cand-pre))
        candidates ])))
(define (sample-candidates pre->ss n-cand counts pre->%)  ;; #(LayerX->{Source}) #(LayerX->Nat) -> {Source}
  ;; produce sources sampled from pre->ss, aiming for %s in l2-counts to match pre->%
  (let ([tot%  (vector-ref pre->% 5)]
        [total (apply fx+ (vector->list counts))])
    (do ( [pre-kind 0 (fx1+ pre-kind)]
          [sample (list)
            (append (sample-pre pre->ss pre-kind counts total tot%
                      n-cand (vector-ref pre->% pre-kind))
              sample)])
        ((fx=? 5 pre-kind) sample))))
  ;; (l2-filter patch sources post-mcx post-ccx proximal)
  (let ([counts     (patch-l2-counts patch)]
        [pre->ss    (make-vector 5 (list))]
        [n-cand     (length sources)])
    ;; partition reachable sources by pre kind (layer)
    (let next-ss ([ss sources] [long-connect (random 100)])
      (when (pair? ss)
        (let* ( [source   (car ss)]
                [pre-kind (source-layer source)]
                [pre-ccx    (source-ccx source)]
                [pre-mcx    (cellx->colx (source-cellx source))]
                [same-cc    (fx=? pre-ccx post-ccx)]
                [distance2  (if same-cc
                              (within-cc-distance2 pre-mcx post-mcx)
                              (mc-distance2 pre-ccx pre-mcx post-ccx post-mcx))]
                [radius     (vector-ref l2-span pre-kind)]
                [reach?     (or (fx<=? distance2 (random radius)) (fxzero? long-connect))])
          (when reach?
            (vector-set! pre->ss pre-kind (cons source (vector-ref pre->ss pre-kind)))))
        (next-ss (cdr ss) (if (fxzero? long-connect)  (random 100)
                              (fx1- long-connect)))))
    ;; sample sources if cc over quota for pre->post connections
    (sort-unique!
      (sample-candidates pre->ss n-cand counts (case post-kind
          [(proximal) ;; distribute ss4l23/p4->p23 according to #cells in each population
                      (let ([l23+p4 (fx+ ss4l23-c/mc p4-c/mc)])
                        (vector 0 0 0 (fxdiv (fx* 100 ss4l23-c/mc) l23+p4)
                                      (fxdiv (fx* 100 p4-c/mc) l23+p4) 100)) ]
          [(distal)   '#(0 100  0   0   0  100)]  ;; p23 distal input is p23 only
        )))))))
                  
(define (l4-filter                       ;; {Source} ColX CCX Layer #(Layer->Nat) -> {Source}
          sources                        ;; pre-synaptic cells
          post-mcx                       ;; minicolumn within cc of post-synaptic cell
          post-ccx                       ;; cortical column of post-synaptic cell
          post-kind                      ;; cell/segment type: ss4l4 ss4l23 p4b p4a
          pre-totals)                    ;; 
  ;; sample sources (pre-synaptic activity) for connection to a target (post-synaptic neuron)
  ;; depends on axon->dendrite reach and specified distribution of connections
  (if (not hex-mc-lattice)
    ;; Numenta experiments: no topology, segregated pre->post connections, fixed sample size
    (filter (lambda (source)
        (let ([pre-kind (source-layer source)])
          (case post-kind
            [(ss4l4)  (fx=? pre-kind 2)]     ;; ss4l4->ss4l4
            [(ss4l23) (fx=? pre-kind 0)]     ;; location->ss4l23
            [(p4b)    (fx=? pre-kind 0)]     ;; location->p4b
            [(p4a)    (fx=? pre-kind 1)])))  ;; p23->p4a
      sources)
    ;; hex lattice: use axon reach /and/ proportionate sampling of all sources
    (let ([pre->ss    (make-vector 5 (list))]
          [n-cand     (length sources)])
      (define (sample-pre pre-kind total %tot %pre)  ;; LayerX Nat Nat Nat -> {Source}
        ;; produce sources (maybe sub-sampled) of pre-kind
        (let* (
            [candidates (vector-ref pre->ss pre-kind)]
            [n-cand-pre (length candidates)]
            [total-pre  (vector-ref pre-totals pre-kind)])
          (cond  ;; sub-sample if cc % over quota
                 ;; (n-cand[pre]/n-cand > pre%/tot%) == n-cand[pre]*tot% > n-cand*pre%
            [(fx>? (fx* total-pre %tot) (fx* total %pre))
              (let ([sample (fxdiv n-cand-pre 5)])
                (vector-set! pre-totals pre-kind (fx+ total-pre sample))
                (list-sample candidates sample)) ]
            [else
              (vector-set! pre-totals pre-kind (fx+ total-pre n-cand-pre))
              candidates ])))
      (define (sample-candidates pre->%)  ;; (Vector LayerX->Nat) -> {Source}
        ;; produce 
        (let ([tot%  (vector-ref pre->% 5)]
              [total (apply fx+ (vector->list pre-totals))])
          (do ( [pre-kind 0 (fx1+ pre-kind)]
                [sample (list)
                  (append (sample-pre pre-kind total tot%
                             (vector-ref pre->% pre-kind))
                    sample)])
              ((fx=? 5 pre-kind) sample))))
      ;; partition reachable sources by pre kind (layer)
      (let next-ss ([ss sources] [long-connect (random 100)])
        (when (pair? ss)
          (let* ( [source   (car ss)]
                  [pre-kind (source-layer source)])
            (let* (
                [pre-ccx    (source-ccx source)]
                [pre-mcx    (cellx->colx (source-cellx source))]
                [same-cc    (fx=? pre-ccx post-ccx)]
                [distance2  (if same-cc
                              (within-cc-distance2 pre-mcx post-mcx)
                              (mc-distance2 pre-ccx pre-mcx post-ccx post-mcx))]
                [radius     (vector-ref (if (eq? post-kind 'p4a) l2-span l4-span) pre-kind)]
                
                [reach?     (or (fx<=? distance2 (random radius)) (fxzero? long-connect))]
                #;
                [reach?     (if same-cc  reach?
                                (and reach? (fxzero? (random 10))))])
              (when reach?
                (vector-set! pre->ss pre-kind (cons source (vector-ref pre->ss pre-kind)))))
            (next-ss (cdr ss) (if (fxzero? long-connect)  (random 100)
                                  (fx1- long-connect))))))
      ;; sample sources if cc over quota for pre->post connections
      (sort-unique!
        (sample-candidates (case post-kind
            [(ss4l4)  '#(10  4 30 4 4   52)]
            [(ss4l23) '#(30  4 10 4 4   52)]
            [(p4b)    '#(30  4 10 4 4   52)]
            [(p4a)    '#( 4 60  0 8 8   80)]
#|
            [(ss4l4)  '#(33  3 12 4 4   56)]
            [(ss4l23) '#(31  6 11 4 4   56)]
            [(p4b)    '#(31  4 12 4 4   55)]
            [(p4a)    '#( 3 63  1 7 8   82)]

            [(ss4l4)  '#(25  0 75  0  0  100)]
            [(ss4l23) '#(75  0 25  0  0  100)]
            [(p4b)    '#(75  0 25  0  0  100)]
            [(p4a)    '#( 0 80  0  0  0   80)]
|#            
            ))))))
                                                                                           ;
;; (make-patch ...)
    ;; (lambda (ncc nmc l2-c/mc ss4l4-c/mc ss4l23-c/mc p4-c/mc l6-c/mc cp-overrides attm-overrides)
    ;; make CCX->Layer vectors, fill connect procs (which need link to patch)
    (assert (fx<=? ncc (expt 2 ccx-bits)))
    (assert (fx>=? 16 (fxmax ss4l4-c/mc ss4l23-c/mc p4-c/mc l6-c/mc l2-c/mc)))
    #;
    (assert (fx<=?                   ;; for cellx->colx by fxdiv
        (fx* nmc (fxmax ss4l4-c/mc ss4l23-c/mc p4-c/mc l6-c/mc l2-c/mc))
        (expt 2 cellx-bits)))
            
    (letrec* (
        [nearby-ccs
          (build-vector ncc (lambda (ccx)
            (if hex-mc-lattice
              (sort fx>?                  ;; descending sort so that sources-for-ccs sorts up
                (filter (lambda (ccy)      ;; (could sample ccs for patchy connectivity)
                    (fx<=? 0 (cc-distance2 ccx ccy) 800))
                  (iota ncc)))
              (list ccx))))]               ;; just the cc if not using hexagonal topology
        [l4-counts (make-vector 5 0)]
        [filter-ss (lambda (sources post-mcx ccx post-kind)
                     (l4-filter sources post-mcx ccx post-kind l4-counts))]
        [p (new
          (build-vector ncc (lambda (ccx)      ;; l2s
            (let ([n-cells (fx* l2-c/mc nmc)])
              (make-l2 (append
                `([cell-count . ,n-cells])  ;; prevent override of cell-count
                cp-overrides
                `([cortical-column      . ,ccx]
                  [lateral-input-widths . 
                    ,(make-list ncc n-cells)]))))))
          (build-vector ncc (lambda (ccx)      ;; l4s
              (make-l4 ccx ss4l4-c/mc ss4l23-c/mc p4-c/mc filter-ss attm-overrides)))
          nearby-ccs
          (make-vector 5 0)
          l4-counts ;(make-vector 5 0)
          (if (fx>? ncc 1)  (make-thread-pool 11)  #f) )])
        (define (proximal-filter sources post-mcx ccx)
          (l2-filter p sources post-mcx ccx 'proximal))
        (define (distal-filter sources post-mcx ccx)
          (l2-filter p sources post-mcx ccx 'distal))
        (do ([ccx 0 (fx1+ ccx)]) ((fx=? ccx ncc))
          (l2-proximal-filter-set! (vector-ref (patch-l2s p) ccx) proximal-filter)
          (l2-distal-filter-set! (vector-ref (patch-l2s p) ccx) distal-filter))
        p
          
          ))) ))
                                                                                            ;
(define (compute p features locations    ;; Patch #(CCX->SDR) #(CCX->SDR) Boolean ->
          learn)
  ;; run one timestep of patch
  (let ([ncc (vector-length features)])
                                                                                            ;
(define (cellxs->sources ccx layerx cellxs)       ;; CCX LayerX {CellX} -> {Source}
  ;; produce source list for one cc/layer; creates new pairs so not changed by compute
  (let ([stride (make-source ccx layerx 0)])
    ;; ie (map (lambda (cellx) (fx+ cellx stride)) cellxs)
    (let next-cx ([cellxs cellxs] [out (list)])
      (cond
        [(null? cellxs) (list-reverse! out) ]
        [else (next-cx (cdr cellxs) (cons (fx+ stride (car cellxs)) out)) ]))))
                                                                                            ;
(define (sources-for-cc ccx proc)        ;; CCX (CCX L4 -> { [LayerX . {CellX}] }) -> {Source}
  ;; produce source list for ccx, combining sources by proc
  ;; note: layerxs and cellxs are arranged so that result is sorted
  (fold-right (lambda (lx+cxs acc)
      (append! (cellxs->sources ccx (car lx+cxs) (cdr lx+cxs)) acc))
    (list)
    (proc ccx (vector-ref (patch-l4s p) ccx))))
                                                                                            ;
(define (sources-within-ccs proc)        ;; (CCX -> { [LayerX . {CellX}] }) -> {{Source}}
  ;; produce list per cc of sources, combining sources by proc
  ;; note: ccx, layerxs, and cellxs are arranged so that result is sorted
  (do ( [ccx (fx1- ncc) (fx1- ccx)]
        [acc (list) (cons                ;; list for each cc...
                      (fold-right (lambda (s acc)
                          (append! (cellxs->sources ccx (car s) (cdr s)) acc))
                        (list)
                        (proc ccx))      ;; ...append layers defined by proc
                      acc)] )
      ((fxnegative? ccx) acc)))
                                                                                            ;
(define (sources-within-nearby-ccs ccx proc) ;; CCX (CCX -> { [LayerX . {CellX}] }) -> {{Source}}
  ;; produce list per nearby cc of ccx of sources, combining sources by proc
  ;; note: nearby-ccs, layerxs, and cellxs are arranged so that result sources are sorted
  (do ( [ccys (vector-ref (patch-nearby-ccs p) ccx) (cdr ccys)]
        [acc (list) (cons                ;; list for each nearby cc...
                      (let ([ccy (car ccys)])
                        (fold-right (lambda (s acc) ;; each source kind
                            (append! (cellxs->sources ccy (car s) (cdr s)) acc))
                          (list)
                          (proc ccy)))   ;; ...append layers defined by proc
                      acc)] )
      ((null? ccys) acc)))
                                                                                            ;
(define (all-sources proc)               ;; (CCX L4 -> { [LayerX . {CellX}] }) -> {Source}
  ;; produce all sources produced by proc
  ;; note: ccx, layerxs, and cellxs are arranged so that result is sorted
  (do ( [ccx (fx1- ncc) (fx1- ccx)]
        [acc (list)
          (append!
            (fold-right (lambda (lx+cxs acc)
                (append! (cellxs->sources ccx (car lx+cxs) (cdr lx+cxs)) acc))
              (list)
              (proc ccx (vector-ref (patch-l4s p) ccx)))
            acc)] )
      ((fxnegative? ccx) acc)))
                                                                                            ;
;; (compute p features locations learn)
  (let* ( [l4-burstings (make-vector ncc)]
          [l4-activity  (all-sources (lambda (ccx l4)
              `([1 . ,(l2-active-cells (vector-ref (patch-l2s p) ccx))]
                [2 . ,(l4-active-cells l4 'ss4l4)]
                [3 . ,(l4-active-cells l4 'ss4l23)]
                [4 . ,(l4-active-cells l4 'p4)]) )) ]
          [l4-learning  (all-sources (lambda (ccx l4)
              `([1 . ,(l2-active-cells (vector-ref (patch-l2s p) ccx))]
                [2 . ,(l4-learning-cells l4 'ss4l4)]
                [3 . ,(l4-learning-cells l4 'ss4l23)]
                [4 . ,(l4-learning-cells l4 'p4)]))) ])
    ;; L4: depolarize, inhibit for all ccs
    (threaded-for-each-x (patch-tp p)
      (lambda (ccx)
        (let ([l4       (vector-ref (patch-l4s p) ccx)]
              [feature  (vector-ref features ccx)]
              [location (map (lambda (cellx) (make-source ccx 0 cellx))
                             (vector-ref locations ccx))])
          (l4-depolarize-cells
            l4
            feature
            location
            l4-activity
            l4-activity
            learn)
          (l4-inhibit-cells l4 feature)))
      ncc)
    ;; L4: activate for all ccs
    (threaded-for-each-x (patch-tp p)
      (lambda (ccx)
        (let ([l4       (vector-ref (patch-l4s p) ccx)]
              [feature  (vector-ref features ccx)]
              [location (map (lambda (cellx) (make-source ccx 0 cellx))
                             (vector-ref locations ccx))])
          (l4-activate-cells l4 
            feature                        ;; proximal input
            location
            l4-activity
            l4-learning
            l4-activity
            l4-learning
            learn)
          (vector-set! l4-burstings ccx (l4-bursting-cols l4))))
      ncc)
    ;; L2 inputs: L4 -> basal (modulatory/"context"), L2 -> proximal (driving/feedforward)
    
    (threaded-for-each-x (patch-tp p)
      (lambda (ccx)
        (let ([l2 (vector-ref (patch-l2s p) ccx)])
          (l2-compute l2
            (sources-for-cc ccx (lambda (ccx l4)           ;; feedforward input
                `([3 . ,(l4-active-cells l4 'ss4l23)]
                  [4 . ,(l4-active-cells l4 'p4)])))
            (sources-within-ccs (lambda (ccx)              ;; context input: all ccs
                `([1 . ,(l2-active-cells (vector-ref (patch-l2s p) ccx))])))
            (sources-for-cc ccx (lambda (ccx l4)           ;; feedforward growth candidates
                `([3 . ,(l4-predicted-active-cells l4 'ss4l23)]
                  [4 . ,(l4-predicted-active-cells l4 'p4)])))
            (list)  ;; context growth candidates (cp will use context-input)
            learn                                          ;; learn?
            (sources-for-cc ccx (lambda (ccx l4)           ;; predicted input
                `([3 . ,(l4-predicted-cells l4 'ss4l23)]
                  [4 . ,(l4-predicted-cells l4 'p4)])))
            (vector-ref l4-burstings ccx))                 ;; bursting columns
            ))
      ncc)))
  (unless collected-static               ;; collect after first iteration
    (set! collected-static #t)
    (collect (collect-maximum-generation) 'static)))
                                                                                            ;
(define collected-static #f)

(define (reset p)
  (vector-for-each  l2-reset  (patch-l2s p))
  (vector-for-each  l4-reset  (patch-l4s p)))
                                                                                            ;
(define (reset-l2 p)
  (vector-for-each  l2-reset  (patch-l2s p)))
                                                                                            ;
(define (reset-l4 p)
  (vector-for-each  l4-reset  (patch-l4s p)))
                                                                                            ;
(define (reset-seq p)
  (vector-for-each  l4-reset-seq  (patch-l4s p)))
                                                                                            ;
(define (print-statistics p)
  (l4-print-statistics (patch-l4s p))
  (l2-print-statistics (patch-l2s p)))

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
