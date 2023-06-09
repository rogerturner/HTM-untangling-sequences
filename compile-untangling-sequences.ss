#! /usr/local/bin/scheme --script
;; HTM-untangling-sequences/compile~ © 2019 Roger Turner
;; SPDX-License-Identifier: AGPL-3.0-or-later
;; see Notices below for License and Contact links

#| Untangling Sequences experiment runner (executable shell script)

To compile and run experiment H3b with default parameters:
  $ compile-untangling-sequences.ss H3b

Changes to number of minicolumns/cc must be set here (creates parameters.ss):
  $ compile-untangling-sequences.ss H3b -nmc 91

Compiled program can be run with different parameters:
  $ scheme --program untangling-sequences.wp H3b -ncc 7

  |#
  
(import
  (chezscheme))
  
(define default-mc/cc 127)
  
(define (do-compile mc/cc exp)         ;; String String String ->
  ;; compile whole program, run  [tag: parameters]
  
  (let* (
      [mc/cc-env (getenv "HTM-scheme-mc/cc")]
      [mc/cc
        (cond
          [mc/cc                               ;; set on command line
            (putenv "HTM-scheme-mc/cc" mc/cc)
            mc/cc ]
          [mc/cc-env                           ;; shell env value
            (string->number mc/cc-env) ]
          [else
            (putenv "HTM-scheme-mc/cc" (number->string default-mc/cc))
            default-mc/cc ]) ])
    (with-output-to-file "parameters.ss" (lambda ()
        (put-string (current-output-port) (string-append
          "#!chezscheme ;; parameters.ss is created by compile-*.ss: do not edit!\n\
           (library (parameters)\n\
             (export\n\
               *mc/cc*\n\
               )\n\
             (import\n\
               (chezscheme))\n\
             (define *mc/cc* " mc/cc ")\n\
          )" )))
      'truncate))
  (parameterize (
      (library-directories '(("." . "object-files") ("../HTM-scheme/" . "object-files")))
      (compile-imported-libraries #t)
      (enable-cross-library-optimization #t)
      (optimize-level 3)
      (debug-level 0)
      (commonization-level 0)
      ;(generate-interrupt-trap #f)
      (generate-inspector-information #f)
      (generate-procedure-source-information #f)
      (#%$optimize-closures #t)
      (#%$lift-closures #t)
      (#%$track-dynamic-closure-counts #f)
      (#%$track-static-closure-counts #f)
      (cp0-effort-limit 2000)              ;; (default is 200) [higher changes o/p?!]
      (cp0-score-limit   200)              ;; (default is 20)  [higher changes o/p?!]
      #;(cp0-outer-unroll-limit 1)         ;; (default is 0) [1 slower]
      #;(#%$loop-unroll-limit 4)           ;; [not effective]
      (generate-wpo-files #t)
      (undefined-variable-warnings #t)
      (generate-allocation-counts #f)
      #;(import-notify #t)
      (compile-file-message #f)
      )
                                                                                            ;
    (compile-program "untangling-sequences.ss" "object-files/untangling-sequences.so")
    (let ([missing
        (compile-whole-program "object-files/untangling-sequences.wpo" "untangling-sequences.wp")])
      (if (null? missing)
        (if (string=? exp "")  (exit)
            (let ([command (string-append "scheme --program untangling-sequences.wp " exp)])
              (display command) (newline)
              (system command)
              (exit)))
        (display missing)))))
                                                                                            ;
(define (main command-line)              ;; {String} ->
  ;; extract parameters: (./compile H3b -nmc 91 -hex f) => (do-compile "91" "f" "H3b")
  (let process ([args command-line] [nmc "127"] [exp ""])
    (cond
      [(null? args)  (do-compile nmc exp) ]
      [(case (car args)
        [("-nmc")  (process (cddr args) (cadr args) exp) ]
        [else      (process (cdr args) nmc (string-append exp " " (car args))) ]) ])))
                                                                                            ;
(main (command-line-arguments))

#| Notices:

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
