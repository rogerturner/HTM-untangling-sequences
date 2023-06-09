#! /usr/local/bin/scheme --libdirs ".:.:../../HTM-scheme/:" --program
;; github.com/rogerturner/.../untangling-sequences.ss © 2019 Roger Turner
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; see *Notices* below for License and Contact links

#| HTM Untangling Sequences experiment (Scheme top-level program)

  Partial replication and extension of experiments reported in Numenta papers
  [Hawkins etal 2017 A Theory of How Columns in the Neocortex Enable Learning
  the Structure of the World](doi:10.3389/fncir.2017.00081) ["Columns paper"]
  [Ahmad & Hawkins 2017 Untangling Sequences: Behavior vs. External Causes]
  (doi:10.1101/190678)

  Defines experiment parameters, calls untangling-sequences-lib to run.
  Load in repl and use (run "exp"):
    $ scheme
    > (load "untangling-sequences.ss")
    > (run "3b -ncc 7")
  
  /or/ compile and run with compile-untangling-sequences.ss

  Indentation facilitates using an editor "Fold All" view for a file overview.

  "Remember that all models are wrong; the practical question is how wrong
  do they have to be to not be useful" [George Box]

  |#

  #!chezscheme

(import
  (chezscheme)
  (parameters)
  (frameworks htm-prelude)
  (frameworks htm-concept)
  (untangling-sequences-lib))

(define exp3b `(                         ;; use untangling-sequences-lib defaults ..
    [figure              . f3b]          ;; .. compile with -nmc 127
    [num-features         .  3]
    [num-points           . 10]
    [num-locations        . 10]
    [num-objects          . 10]
    [num-repetitions      .  5]
    [ss4l4-c/mc           .  0]
    [ss4l23-c/mc          .  0]
    [p4-c/mc              . 15]
    [use-bursting-columns . #f]
    [num-sequences        .  0]
    [train-keys           . (l2t)]
    [test-keys            . (l2a)]))

  #|
  "Columns" paper, figure 3, *htmpapers repository*:
  https://github.com/numenta/htmpapers/tree/master/frontiers/a_theory_of_how_columns_in_the_neocortex_enable_learning_the_structure_of_the_world
  |#
(define expH3b `(
    [figure              . H3b]          ;; htmpapers/.../convergence_activity.py#runExperiment
    [num-features         .  3]
    [num-points           . 10]
    [num-locations        . 10]
    [num-objects          . 10]
    [num-rpts-per-sensation . 2]         ;; ?
    [num-input-bits       . 20]
    [sensor-input-size    . 1024]
    [external-input-size  . 2400 #;1024]
                                         ;; (override defaults in untangling_sequences-lib#experiment)
    [l2-overrides         . (            ;; htmresearch/.../l2_l4_inference.py#getDefaultL2Params
      [sample-size-proximal        . 10]
      [min-threshold-proximal      . 5]
      [cell-count                  . 4096]
      [sdr-size                    . 40]
      [min-sdr-size                . 39]
      [max-sdr-size                . 41]
      [initial-distal-permanence   . ,(perm 0.41)]
      [activation-threshold-distal . 13]
      [sample-size-distal          . 20]
      [connected-permanence-distal . ,(perm 0.5)]
      [inertia-factor              . ,(fx3<- 0.97)]
      [connect?                    . #f] )]
    [l4-overrides         . (            ;; htmresearch/.../l2_l4_inference.py#getDefaultL4Params
      [activation-threshold    . 13]
      [min-threshold           . 13]
      [initial-permanence      . ,(perm 0.51)]
      [connected-permanence    . ,(perm 0.6)]
      [permanence-decrement    . ,(perm 0.02)]
      [predicted-segment-decrement . ,(perm 0.0)]
      [reduced-basal-threshold . ,(int<- (* 13 0.6))]
      [sample-size             . ,(int<- (* 1.5 20))]
      [connect?                . #f] )]
    [num-learning-points  .  3]          ;; ? htmresearch/.../l2_l4_inference.py#__init__
    [num-repetitions      .  5]          ;; 5 required
    [ss4l4-c/mc           .  0]
    [ss4l23-c/mc          .  0]
    [p4-c/mc              . 16]
    [use-bursting-columns . #f]
    [num-sequences        .  0]
    [train-keys           . (l2t)]
    [test-keys            . (l2a)]))
                                                                                            ;
  #|
  "Columns" paper, figure 3, *as published*: doi:10.3389/FNCIR.2017.00081
  |#
(define expF3b `(
    [figure              . F3b]          ;; Simulation Results, p6
    [num-points           . 10]          ;; "Each object consists of 10 sensory features.."
    [num-features         .  5]          ;; "..library of 5 to 30 possible features"
    [num-locations        . 10]
    [num-objects          . 10]          ;; ?
    [num-rpts-per-sensation . 2]         ;; ?
    [num-input-bits       . 10]
    [sensor-input-size    . 150]
    [external-input-size  . 2400]
                                         ;; (override defaults in untangling_sequences-lib#experiment)
    [l2-overrides         . (            ;; Simulation Details, p15 [htmresearch/.../l2_l4_inference.py#getDefaultL2Params]
      [sample-size-proximal        . 10] ;; ?
      [min-threshold-proximal      . 3]  ;; "..activation threshold is 3 for proximal dendrites"
      [cell-count                  . 4096]
      [sdr-size                    . 40]
      [min-sdr-size                . 0]
      [initial-distal-permanence   . ,(perm 0.41)]
      [activation-threshold-distal . 18] ;; "..and 18 for basal dendrites for output neurons"
      [sample-size-distal          . 20]
      [connected-permanence-distal . ,(perm 0.5)]
      [connect?                    . #f] )]
    [l4-overrides         . (            ;; Simulation Details, p15 [htmresearch/.../l2_l4_inference.py#getDefaultL4Params]
      [activation-threshold    . 6]      ;; "..activation threshold of basal distal dendrite of input neuron is 6"
      [min-threshold           . 6]
      [initial-permanence      . ,(perm 0.51)]
      [connected-permanence    . ,(perm 0.6)]
      [permanence-decrement    . ,(perm 0.02)]
      [predicted-segment-decrement . ,(perm 0.0)]
      [reduced-basal-threshold . ,(int<- (* 6 0.6))]
      [sample-size             . ,(int<- (* 1.5 10))]
      [connect?                . #f] )]
    
    [num-learning-points  .  3]          ;; ? htmresearch/.../l2_l4_inference.py#__init__
    [num-repetitions      .  5]
    [ss4l4-c/mc           .  0]
    [ss4l23-c/mc          .  0]
    [p4-c/mc              . 16]
    [use-bursting-columns . #f]
    [num-sequences        .  0]
    [train-keys           . (l2t)]
    [test-keys            . (l2a)]))
                                                                                            ;
(define exp4a '(
    [figure         .  f4a]
    [num-sequences  .    5]
    [num-objects    .    0]
    [num-features   .   10]
    [num-locations  .  100]
    [num-repetitions .   20]
    [train-keys     . (l4ac TMac TXac)]
    [test-keys      . (l4lnp l4pa TMlnp TMpa TXlnp TXpa)]))
                                                                                            ;
(define exp5a '(
    [figure         .  f5a]
    [num-objects    .   50]
    [num-sequences  .    0]
    [num-features   .  100]
    [num-locations  .  100]
    [train-keys     . (l4ac TMac TXac)]
    [test-keys      . (l4lnp l4pa TMlnp TMpa TXlnp TXpa)]))
                                                                                            ;
(define exp6 '(
    [figure          .  f6]
    [num-sequences   .  50]
    [num-objects     .  50]
    [num-features    .  50]
    [num-locations   . 100]
    [num-repetitions .  32]
    [interleave-training   . #t]
    [random-seq-location   . #t]
    [location-per-sequence . #f]
    [train-keys      . (l4ac TMac TXac)]
    [test-keys       . #;(l4pa TMpa TXpa) (l4lpa TMlpa TXlpa)]))
                                                                                            ;
(define (exec exp times options)         ;; String Nat [ {KWarg} ] ->
  ;; run experiment with options
  ;(collect (collect-maximum-generation) 'static)
  ;(heap-reserve-ratio 2.0)
  (collect-trip-bytes (expt 2 24))
  (let ([start-stats (statistics)]
        [start-time  (cpu-time)])
    (do ([ex 1 (fx1+ ex)]) ((fx>? ex times))
      (experiment
        (case exp
          [("3b")    exp3b ]
          [("H3b")   expH3b]
          [("F3b")   expF3b]
          [("4a")    exp4a ]
          [("5a")    exp5a ]
          [("6")     exp6  ])
        options
        #;
        (cons (cons 'display-timing (fx=? ex times))
              options)))
    ;(enable-object-counts #t)
    ;(collect)
    ;(display (object-counts))
    (display-thunk-time)
    (let ([tenths (div (+ (- (cpu-time) start-time) 50) 100)])
      (for-each display `(,(div tenths 10) "." ,(mod tenths 10) "s thread time\n")))
    (sstats-print (sstats-difference (statistics) start-stats))))
                                                                                            ;
(define (option name parameter)          ;; String [Number | "f" | X] -> KWarg
  ;; produce key/value from arg
  (define (number) (string->number parameter))
  (define (boolean)
    (case parameter
      [("f") #f]
      [else #t]))
  (case name
    [("-nib")   (cons 'num-input-bits        (number))]
    [("-l4")    (cons 'ss4l4-c/mc            (number))]
    [("-l23")   (cons 'ss4l23-c/mc           (number))]
    [("-p4")    (cons 'p4-c/mc               (number))]
    [("-in")    (cons 'intersperse-noise     (number))]
    [("-it")    (cons 'interleave-training   (boolean))]
    [("-lps")   (cons 'location-per-sequence (boolean))]
    [("-ncc")   (cons 'num-cortical-columns  (number))]
    [("-nf")    (cons 'num-features          (number))]
    [("-nl")    (cons 'num-locations         (number))]
    [("-no")    (cons 'num-objects           (number))]
    [("-nr")    (cons 'num-repetitions       (number))]
    [("-ns")    (cons 'num-sequences         (number))]
    [("-ol")    (cons 'online-learning       (boolean))]
    [("-rsl")   (cons 'random-seq-location   (boolean))]
    [("-seed")  (cons 'seed                  (number))]
    [("-ubc")   (cons 'use-bursting-columns  (boolean))]
    [("-ss")    (cons 'superimpose-sequence  (boolean))]))
                                                                                            ;
(define (parse command-line)              ;; {String} -> String {KWarg}
  ;; eg $ scheme --program untangling-sequences.wp 6 -nmc 150 -it f
  (let process ([args (reverse command-line)] [times 1]
                [options (list)] [parameter "t"])
    (let ([arg  (car args)]
          [rest (cdr args)])
      (cond
        [(or (null? args)
             (string=? "" arg)) ]   ;; (not run as --program)
        [(string=? "-ne" arg)
          (process rest (string->number parameter) options "t") ]
        [(null? rest)
          (values arg times options)]     ;; last (first) element is experiment
        [(char=? #\- (string-ref arg 0))
          (process rest times (cons (option arg parameter) options) "t")]
        [else
          (process rest times options arg)]))))
                                                                                            ;
(define (run command)                    ;; String ->
  ;; command is eg: "F3b -seed 42"
  (call-with-values
    (lambda _ (parse (string->word-list command)))
    exec))

(display (examples-checked " examples checked\n"))

(let ([args (command-line)])             ;; $ untangling-sequences.ss F3b -seed 42
  ;; first arg is "untangling-sequences.ss"
  (when (> (length args) 1)
    (call-with-values
      (lambda _ (parse (cdr (command-line))))
      exec)))
    
#;(profile-dump-data "untangling-sequences.data") 

  #|
  % scheme --program untangling-sequences.wp H3b -ncc 1000
  618.5s thread time; H3b (seed . 879)(num-cortical-columns . 1000);
  10000 sdrs created
  syns/segments   20908176/191425 p23 proximal
                  3997622/191425 p23 distal
  syns/segs/cells 1225429/149847/2032000 p4
                  4975959/483820/2032000 p4
  pre-index (avg) 0 ss4l4
                  0 ss4l23
                  16.7 p4
                  10.0 p4
      544 collections
      4.331060791s elapsed cpu time, including 73.234253000s collecting
      69.854570000s elapsed real time, including 11.128768000s collecting
      73028556960 bytes allocated, including 72222140704 bytes reclaimed
  |#
 
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
