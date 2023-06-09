;; github.com/rogerturner/.../column-pooler.ss © 2019 Roger Turner
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; see *Notices* below for License and Contact links

#| 

Translated from github.com/numenta/htmresearch/.../column_pooler.py ("cp.py")
see comments there for descriptions of functions and parameters.

Note that elements of input SDRs are Source indexes (include CC and Layer index).

*See htm-concept.ss for type and data structure descriptions and code conventions*
  
Selected comments from cp.py:                                                                                       ;

  This class constitutes a temporary implementation for a cross-column pooler.
  The implementation goal of this class is to prove basic properties before
  creating a cleaner implementation.

  maxSdrSize
    The maximum SDR size for learning.  If the column pooler has more
    than this many cells active, it will refuse to learn. This serves
    to stop the pooler from learning when it is uncertain of what object
    it is sensing.

  minSdrSize
    The minimum SDR size for learning. If the column pooler has fewer
    than this many active cells, it will create a new representation
    and learn that instead. This serves to create separate
    representations for different objects and sequences.
    If online learning is enabled, this parameter should be at least
    inertiaFactor*sdrSize.  Otherwise, two different objects may be
    incorrectly inferred to be the same, as SDRs may still be active
    enough to learn even after inertial decay.

  sampleSizeProximal
    Number of proximal synapses a cell should grow to each feedforward
    pattern, or -1 to connect to every active bit

  predictedInhibitionThreshold
    How much predicted input must be present for inhibitory behavior
    to be triggered. Only has effects if onlineLearning is true.

  sampleSizeDistal
    Number of distal synapses a cell should grow to each lateral
    pattern, or -1 to connect to every active bit

  inertiaFactor
    The proportion of previously active cells that remain
    active in the next timestep due to inertia (in the absence of
    inhibition). If onlineLearning is enabled, should be at most
    1 - learningTolerance, or representations may incorrectly become
    mixed.
    
  Learning mode: 

    If there was prior activity, we maintain it.

    If there are not enough previously active cells, then we are no longer on
    a familiar object. Either our representation decayed due to the passage
    of time (i.e. we moved somewhere else) or we were mistaken. Either way,
    create a new SDR and learn on it: randomly activate 'sdrSize' cells and create
    connections to incoming input.
    This case is the only way different object representations are created.
    These cells will represent the object and learn distal connections to each
    other and to lateral cortical columns.

    If we have a union of cells active, don't learn. This primarily affects
    online learning.

  Inference mode: 

    If there is some feedforward activity, perform
    spatial pooling on it to recognize previously known objects, then use
    lateral activity to activate a subset of the cells with feedforward
    support. 
    If there is no feedforward activity, use lateral activity to
    activate a subset of the previous active cells.

    First, activate the FF-supported cells that have the highest number of
    lateral active segments (as long as it's not 0)
    If we haven't filled the sdrSize quorum, add in inertial cells.

    We sort the previously-active cells by number of active lateral
    segments (this really helps). We then activate them in order of
    descending lateral activation.

    We use inertiaFactor to limit the number of previously-active cells
    which can become active, forcing decay even if we are below quota.

    Activate groups of previously active cells by order of their lateral
    support until we either meet quota or run out of cells.
    (on first touch activate all with feedforward support)

    If we haven't filled the sdrSize quorum, add cells that have feedforward
    support and no lateral support.

    Inhibit cells proportionally to the number of cells that have already
    been chosen. If ~0 have been chosen activate ~all of the feedforward
    supported cells. If ~sdrSize have been chosen, activate very few of
    the feedforward supported cells.

    Use the discrepancy:sdrSize ratio to determine the number of cells to
    activate.

    |#

  #!chezscheme

(library (algorithms column-pooler)
                                                                                            ;
(export
make-cp
cp-compute
cp-reset
  cp-n-sdrs
  number-of-proximal-synapses
  number-of-proximal-segments
  number-of-distal-synapses
  number-of-distal-segments
  number-of-lateral-synapses
  number-of-lateral-segments
  (rename ;; for cp.py compatibility
    (cp-input-width   number-of-inputs)
    (cp-cell-count    number-of-cells)
    (cp-sdr-size      get-sdr-size)
    (cp-active-cells  get-active-cells))
  num-connected-proximal-synapses
  cp-min-sdr-size
  cp-max-sdr-size
  cp-cortical-column
  cp-proximal-permanences
  cp-proximal-filter-set!
  cp-distal-filter-set!)
                                                                                            ;
(import
  (except (chezscheme) reset)
          (frameworks htm-prelude)
          (frameworks htm-concept)
          (frameworks coordinates))
                                                                                            ;
  (implicit-exports #f)

;; === Layer record ===
                                                                                            ;
(define cp-defaults `(                   ;; {KWarg} [cf __init__ in cp.py]
  [input-width                    . 0]
  [lateral-input-widths           . ()]
  [cell-count                     . 4096]
  [sdr-size                       . 40]
  [online-learning                . #f]
  [max-sdr-size                   . 0]
  [min-sdr-size                   . 0]
  [syn-perm-proximal-inc          . ,(perm 0.1)]
  [syn-perm-proximal-dec          . ,(perm 0.001)]
  [initial-proximal-permanence    . ,(perm 0.6)]
  [sample-size-proximal           . 20]
  [min-threshold-proximal         . 10]
  [connected-permanence-proximal  . ,(perm 0.50)]
  [predicted-inhibition-threshold . 20]
  [syn-perm-distal-inc            . ,(perm 0.1)]
  [syn-perm-distal-dec            . ,(perm 0.001)]
  [initial-distal-permanence      . ,(perm 0.6)]
  [sample-size-distal             . 20]
  [activation-threshold-distal    . 13]
  [connected-permanence-distal    . ,(perm 0.50)]
  [inertia-factor                 . ,(fx3<- 1.0)]
  [seed                           . 42]
  [active-cells                   . ()]
  [proximal-permanences           . #()]
  [internal-distal-permanences    . #()]
  [distal-permanences             . (#())]
  [use-inertia                    . #t]
  [cortical-column                . 0]
  [proximal-filter              . #f]
  [distal-filter                . #f]
  [n-sdrs                         . 0]))
                                                                                            ;
(define-record-type cp (fields           ;; CP
  input-width
  lateral-input-widths
  cell-count
  sdr-size
  online-learning
  max-sdr-size
  min-sdr-size
  syn-perm-proximal-inc                  ;; Proximal
  syn-perm-proximal-dec
  initial-proximal-permanence
  sample-size-proximal
  min-threshold-proximal
  connected-permanence-proximal
  predicted-inhibition-threshold
  syn-perm-distal-inc                    ;; Distal
  syn-perm-distal-dec
  initial-distal-permanence
  sample-size-distal
  activation-threshold-distal
  connected-permanence-distal
  inertia-factor                         ;; Fixnum3
  seed
  (mutable active-cells)
  proximal-permanences                   ;; CellVecOf Segment
  internal-distal-permanences            ;; CellVecOf Segment
  (mutable distal-permanences)           ;; {CellVecOf Segment}
  use-inertia
  cortical-column
  (mutable proximal-filter)
  (mutable distal-filter)
  (mutable n-sdrs))
  (sealed #t) (opaque #t) (nongenerative cp)
(protocol #;(make-cp cp-params)          ;; {KWarg} -> CP
  (lambda (new)
    (lambda (cp-params)
      (let* (
          [cp         (apply new (key-word-args cp-params cp-defaults))]
          [cp-params  (if (fxpositive? (cp-max-sdr-size cp))  cp-params
                          (cons `[max-sdr-size . ,(cp-sdr-size cp)] cp-params))]
          [cp-params  (if (fxpositive? (cp-min-sdr-size cp))  cp-params
                          (cons `[min-sdr-size . ,(cp-sdr-size cp)] cp-params))]
          [make-permanences (lambda _
              (build-vector (cp-cell-count cp) (lambda (cellx)
                  (make-seg 0 cellx))))]
          [cp-params  (append `(
                          [proximal-permanences        . ,(make-permanences)]
                          [internal-distal-permanences . ,(make-permanences)])
                        cp-params)]
          [laterals   (cp-lateral-input-widths cp)]
          [cp-params  (if (or (null? laterals) (fxzero? (car laterals)))  cp-params
                          (cons `[distal-permanences .
                                   ,(build-list (length laterals) make-permanences)]
                            cp-params))])
        (random-seed (cp-seed cp))
        (apply new (key-word-args cp-params cp-defaults)))))))

;; === Column Pooler algorithm ===
                                                                                            ;
(define  cp-compute (case-lambda         ;; ...
[(cp feedforward-input learn)            ;; CP {Source} Boolean ->
  (cp-compute cp feedforward-input '() '() learn '()) ]
[(cp feedforward-input lateral-inputs    ;; CP {Source} {{Source}} {Source} Boolean ->
    feedforward-growth-candidates learn)
  (cp-compute cp feedforward-input lateral-inputs feedforward-growth-candidates learn '()) ]
[(cp feedforward-input lateral-inputs    ;; CP {Source} {{Source}} {Source} Boolean {Source} ->
     feedforward-growth-candidates learn predicted-input)
  ;; run one time step of the column pooler algorithm
  (let ([feedforward-growth-candidates
          (if (null? feedforward-growth-candidates) feedforward-input
              feedforward-growth-candidates)])
    (if (not learn)                      ;; inference step
      (compute-inference-mode cp feedforward-input lateral-inputs)
      (if (not (cp-online-learning cp))  ;; learning step
        (compute-learning-mode cp feedforward-input lateral-inputs 
          feedforward-growth-candidates)
        (cond                            ;; online learning step
          [(fx>? (length predicted-input) (cp-predicted-inhibition-threshold cp))
            (let ([predicted-active-input
                    (intersect1d feedforward-input predicted-input)]
                  [predicted-growth-candidates
                    (intersect1d feedforward-growth-candidates predicted-input)])
              (compute-inference-mode cp predicted-active-input lateral-inputs)
              (compute-learning-mode cp predicted-active-input lateral-inputs
                predicted-growth-candidates #;feedforward-growth-candidates)) ]
          [(not (fx<=? (cp-min-sdr-size cp)
                       (length (cp-active-cells cp))
                       (cp-max-sdr-size cp)))
            ;; if no single representation, try to infer one before attempting to learn
            (compute-inference-mode cp feedforward-input  lateral-inputs)
            (compute-learning-mode cp feedforward-input lateral-inputs
              feedforward-growth-candidates) ]
          [else 
            ;; no predicted input and single SDR, extend that representation
            (compute-learning-mode cp feedforward-input lateral-inputs
              feedforward-growth-candidates) ])))) ]))
                                                                                            ;
(define (compute-learning-mode cp        ;; CP {Source} {{Source}} {Source} ->
          feedforward-input lateral-inputs feedforward-growth-candidates)
  ;; in learning mode, maintain prior activity or create random sdr for new object
  (define (new-sdr)                      ;; -> SDR
    ;; produce random sdr with sdr-size bits
    (when (and (fxzero? (cp-cortical-column cp)) (fxzero? (cp-n-sdrs cp)))
      (random-seed (cp-seed cp)))        ;; first sdr of first column is deterministic
    (cp-n-sdrs-set! cp (fx1+ (cp-n-sdrs cp)))
    (sort-unique! (u32-sample (iota (cp-cell-count cp)) (cp-sdr-size cp)) (cp-sdr-size cp)))
  ;; (compute-learning-mode)
  (let* ( [prev-active-cells (cp-active-cells cp)]
          [active-sdr-size   (length prev-active-cells)])
    (when (cond
        [ (fx<? active-sdr-size (cp-min-sdr-size cp))
            ;; not enough active cells: create a new sdr and learn on it
            (cp-active-cells-set! cp (new-sdr))
            #t ]
        [ (fx>? active-sdr-size (cp-max-sdr-size cp))
            ;; union of cells active: don't learn
            #f ]
        [ else #t ] )
      ;; do the actual learning: proximal:
      (learn cp (cp-proximal-permanences cp) (cp-active-cells cp) 
        feedforward-input
        feedforward-growth-candidates
        (cp-sample-size-proximal cp) (cp-initial-proximal-permanence cp)
        (cp-syn-perm-proximal-inc cp) (cp-syn-perm-proximal-dec cp)
        (cp-proximal-filter cp))
      ;; lateral:
      (do ( [ccx 0 (fx1+ ccx)]
            [lis lateral-inputs (cdr lis)]
            [dps (cp-distal-permanences cp) (cdr dps)])
          ((null? lis))
        (unless (fx=? ccx (cp-cortical-column cp))
          (learn cp (car dps)
            (cp-active-cells cp) (car lis) (car lis)
            (cp-sample-size-distal cp) (cp-initial-distal-permanence cp)
            (cp-syn-perm-distal-inc cp) (cp-syn-perm-distal-dec cp)
            (cp-distal-filter cp))))
      ;; distal (this cc):
      (let* ( [ccx     (cp-cortical-column cp)]
              [prev-ss (map (lambda (cellx) (make-source ccx 1 cellx))
                         prev-active-cells)])
        (learn cp (cp-internal-distal-permanences cp)
          (cp-active-cells cp) prev-ss prev-ss
          (cp-sample-size-distal cp) (cp-initial-distal-permanence cp)
          (cp-syn-perm-distal-inc cp) (cp-syn-perm-distal-dec cp)
          (cp-distal-filter cp))))))
                                                                                            ;
(define (compute-inference-mode cp       ;; CP {Source} {{Source}} ->
          feedforward-input lateral-inputs)
  ;; in inference mode, if there is some feedforward activity, recognize previously
  ;; known objects and activate a subset of the cells with feedforward support;
  ;; otherwise use lateral activity to activate a subset of the previous active cells
  (define (extend-active active          ;; {CellX} {CellX} {Nat} Boolean -> {CellX}
            cells num-active-segs-for-cells allow-zero)
    ;; extend active with cells in order of activation until sdrSizequota is reached
    (let loop ( [ttop (apply fxmax num-active-segs-for-cells)]
                [active active] )
      (if (and (if allow-zero  (not (fxnegative? ttop))  (fxpositive? ttop))
               (fx<? (length active) (cp-sdr-size cp)))
        (loop (fx1- ttop)
              (append
                (fold-left               ;; filter cells with activation = current ttop
                  (lambda (filtered cellx nasfc)
                    (if (fx=? nasfc ttop)
                        (cons cellx filtered)
                        filtered))
                  (list)
                  cells  num-active-segs-for-cells)
                active))
        (list-sort fx<? active))))
  ;; (compute-inference-mode)
  (let* (
      [prev-active-cells (cp-active-cells cp)]
      [overlaps          (compute-overlaps (cp-proximal-permanences cp)
                           feedforward-input (cp-connected-permanence-proximal cp))]
      [feedforward-supported-cells
                         (indices-where fx>=? overlaps (cp-min-threshold-proximal cp))]
      [num-active-segments-by-cell
                         (make-vector (cp-cell-count cp) 0)])
    (let (
        [overlaps (compute-overlaps (cp-internal-distal-permanences cp)
                    (let ([ccx (cp-cortical-column cp)])
                      (map (lambda (cellx)
                          (make-source ccx 1 cellx))
                        prev-active-cells))
                    (cp-connected-permanence-distal cp))])
      (increment-where! num-active-segments-by-cell 
                        fx>=? overlaps (cp-activation-threshold-distal cp)))
    (when (pair? lateral-inputs)
      (for-each
        (lambda (lateral-input distal-permanence)
          (let ([overlaps (compute-overlaps distal-permanence
                            lateral-input (cp-connected-permanence-distal cp))])
            (increment-where! num-active-segments-by-cell 
                              fx>=? overlaps (cp-activation-threshold-distal cp))))
        lateral-inputs (cp-distal-permanences cp)))
    (let ([chosen-cells (list)])
      (unless (null? feedforward-supported-cells)
        (let ([num-active-segs-for-ff-sup-cells
                (list-of (vector-ref num-active-segments-by-cell x)
                         (x in feedforward-supported-cells))])
          (set! chosen-cells (extend-active chosen-cells  ;; exclude cells with 0 active segments
                               feedforward-supported-cells num-active-segs-for-ff-sup-cells #f))))
      (when (and (cp-use-inertia cp) (fx<? (length chosen-cells) (cp-sdr-size cp)))
        (let* ( [prev-cells   (setdiff1d prev-active-cells chosen-cells)]
                [inertial-cap (fxdiv (fx* (length prev-cells) (cp-inertia-factor cp)) fx3)])
          (when (fxpositive? inertial-cap)
            (let* ( [num-active-segs-for-prev-cells 
                      (list-of (vector-ref num-active-segments-by-cell x)
                               (x in prev-cells))]
                    [sort-indices (list-sort (lambda (x y)
                          (fx>? (list-ref num-active-segs-for-prev-cells x)
                                (list-ref num-active-segs-for-prev-cells y)))
                        (indexes num-active-segs-for-prev-cells))]
                    [prev-cells (take inertial-cap
                        (list-of (list-ref prev-cells x) (x in sort-indices)))]
                    [num-active-segs-for-prev-cells (take inertial-cap
                        (list-of (list-ref num-active-segs-for-prev-cells x) 
                                 (x in sort-indices)))])
              (set! chosen-cells (extend-active chosen-cells
                                   prev-cells num-active-segs-for-prev-cells #t))))))
      (let ([discrepancy (fx- (cp-sdr-size cp) (length chosen-cells))])
        (when (fxpositive? discrepancy)
          (let* ( [rem-ff-cells (setdiff1d feedforward-supported-cells chosen-cells)]
                  [n  (fxdiv (fx* (length rem-ff-cells) discrepancy) (cp-sdr-size cp))]
                  [n  (fxmin (fxmax n discrepancy) (length rem-ff-cells))])
            (if (fx>? (length rem-ff-cells) n)
              (let ([selected (u32-sample rem-ff-cells n)])
                (set! chosen-cells (append selected chosen-cells)))
              (set! chosen-cells (append rem-ff-cells chosen-cells))))))
      (cp-active-cells-set! cp (sort-unique! chosen-cells)))))
                                                                                            ;
(define (num-connected-proximal-synapses ;; CP {CellX} -> Nat
          cp cells)
  ;; produce total count of synapses above threshold of cells
  (let ([threshold (cp-connected-permanence-proximal cp)])
    (fold-left (lambda (total cellx)
        (fold-left (lambda (total synapse)
            (if (fx>=? (syn-perm synapse) threshold)
              (fx1+ total)
              total))
          total
          (seg-synapses->list (vector-ref (cp-proximal-permanences cp) cellx))))
      0
      cells)))
                                                                                            ;
(define (number-of-synapses cp           ;; CP Permanences {CellX} -> Nat
          permanences cells)
  ;; produce total count of synapses for cells
  (vector-fold-left (lambda (total seg)
      (fx+ total (synapses-length (seg-synapses seg))))
    0
    (if (null? cells)  (permanences cp) 
        (vector-refs (permanences cp) (list->vector (car cells))))))
                                                                                            ;
(define (number-of-proximal-synapses cp  ;; CP [{CellX}] -> Nat
          . cells)
  (number-of-synapses cp cp-proximal-permanences cells))
                                                                                            ;
(define (number-of-proximal-segments cp) ;; CP -> Nat
  (vector-count (lambda (seg)
      (fxpositive? (synapses-length (seg-synapses seg))))
    (cp-proximal-permanences cp)))
                                                                                            ;
(define (number-of-distal-synapses cp    ;; CP [{CellX}] -> Nat
          . cells)
  (number-of-synapses cp cp-internal-distal-permanences cells))
                                                                                            ;
(define (number-of-distal-segments cp)   ;; CP -> Nat
  (vector-count (lambda (seg)
      (fxpositive? (synapses-length (seg-synapses seg))))
    (cp-internal-distal-permanences cp)))
                                                                                            ;
(define (number-of-lateral-synapses cp)  ;; CP -> Nat
  (fold-left (lambda (total col)
      (fx+ total
        (vector-fold-left (lambda (total seg)
            (fx+ total (synapses-length (seg-synapses seg))))
          0
          col)))
    0
    (cp-distal-permanences cp)))
                                                                                            ;
(define (number-of-lateral-segments cp)  ;; CP -> Nat
  (fold-left (lambda (total col)
      (fx+ total
        (vector-count (lambda (seg)
            (fxpositive? (synapses-length (seg-synapses seg))))
          col)))
    0
    (cp-distal-permanences cp)))
                                                                                            ;
(define (cp-reset cp)                    ;; CP ->
  ;; when learning this signifies we are to learn a unique new object
  (cp-active-cells-set! cp (list)))
                                                                                            ;
(define (learn cp permanences            ;; CP Permanences {CellX} {Source} {Source} Nat Perm Perm Perm ->
          active-cells active-input growth-candidate-input
          sample-size initial-permanence permanence-increment permanence-decrement filter-ss)
  ;; for each active cell, reinforce active synapses, punish inactive synapses,
  ;; and grow new synapses to input bits that the cell isn't already connected to
  (let ([active-input (list->u32-vector active-input)])
    (adapt-synapses permanences active-cells active-input
        permanence-increment permanence-decrement)
    (grow-synapses cp permanences active-cells active-input growth-candidate-input
        (if (fxnegative? sample-size)  (greatest-fixnum)  sample-size )
        initial-permanence filter-ss)))

;; === Synapses and Permanences ===
;; ("Permanences" in cp.py correspond to Segments in HTM-scheme)
                                                                                            ;
(define (adapt-synapses permanences      ;; Permanences {CellX} (U32vecOf Source) Perm Perm ->
          active-cells active-input permanence-increment permanence-decrement)
  ;; update synapses: strengthen those connected to input, weaken others, remove on zero
  (for-each (lambda (cellx)
      (synapses-update! (lambda (synapse)
          (let* ( [source  (syn-source synapse)]
                  [perm    (if (u32-search active-input source)
                             (clip-max (fx+ (syn-perm synapse) permanence-increment))
                             (fx- (syn-perm synapse) permanence-decrement))])
            (and (fxpositive? perm) (make-syn source perm))))
        (vector-ref permanences cellx)))
    active-cells))
                                                                                            ;
(define (grow-synapses cp permanences    ;; CP Permanences {CellX} (U32vecOf Source) {Source} {Nat} Perm ({Source} -> {Source}) ->
          active-cells active-input growth-candidates sample-size initial-perm filter-ss)
  ;; create new synapses for some growth-candidates ('setRandomZerosOnOuter' in cp.py)
  (for-each (lambda (cellx)
      (let* (
          [post-mcx (cellx->colx cellx)]
          [segment  (vector-ref permanences cellx)]
          [growth-candidates
                    (if filter-ss
                      (filter-ss growth-candidates post-mcx (cp-cortical-column cp))
                      (remp (lambda (gc) (synapses-search gc segment)) growth-candidates))]
          [max-new
                    (fx- sample-size
                         (synapses-count-if (lambda (synapse)
                             (u32-search active-input (syn-source synapse)))
                           segment))]
          [n-new    (length growth-candidates)])
        (when (and (fxpositive? max-new) (fxpositive? n-new))
          (synapses-merge!                 ;; [ref: merge-ss-not-null]
            (if (fx<=? n-new max-new)  growth-candidates
              (list-sample growth-candidates max-new n-new))
            initial-perm segment))))
    active-cells))
                                                                                            ;
(define (compute-overlaps                ;; Permanences {Source} Perm -> (Vector CellX->Nat)
          permanences sources threshold)
  ;; produce counts of overlaps between synapses above threshold and inputs
  ;; "rightVecSumAtNZGteThresholdSparse" in cp.py
  (vector-map (lambda (segment)
      (fold-left (lambda (overlaps source)
          (let ([synapse (synapses-search source segment)])
            (if (and synapse (fx>=? (syn-perm synapse) threshold))
              (fx1+ overlaps)
              overlaps)))
        0
        sources))
    permanences))
    
;; === Vector utilities ===
                                                                                            ;
(define (indices-where pred? vec value)  ;; (X Y -> Boolean) (Vectorof X) Y -> {Nat}
  ;; produce sorted list of indices of vec elements for which (pred? element value)
  (do ( [vx (fx1- (vector-length vec)) (fx1- vx)]
        [result (list) (if (pred? (vector-ref vec vx) value)
                         (cons vx result)
                         result)] )
      ((fxnegative? vx) result)))
                                                                                            ;
(define (increment-where! cs pred? xs y) ;; (Vectorof Nat) (X Y -> Boolean) (Vectorof X) Y ->
  ;; mutates cs, incrementing elements corresponding to xs for which (pred? x y)
  (do ( [vx 0 (fx1+ vx)] )
      ((fx=? vx (vector-length xs)))
    (when (pred? (vector-ref xs vx) y)
      (vector-set! cs vx (fx1+ (vector-ref cs vx))))))
                                                                                            ;
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
