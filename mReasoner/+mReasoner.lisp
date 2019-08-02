; ---------------------------------------------------------------------------------
; mReasoner is a unified computational implementation of the mental model theory of
; thinking and reasoning.
; ---------------------------------------------------------------------------------
; Copyright (C) 2017 Mental Models and Reasoning Lab
;
; Website: http://mentalmodels.princeton.edu
; Contact: Sangeet Khemlani (skhemlani@gmail.com)
;          Phil Johnson-Laird (phil@princeton.edu)
; ---------------------------------------------------------------------------------
; mReasoner is licensed under a Creative Commons Attribution-NonCommercial-
; ShareAlike 4.0 International License. You are free to:
;
;            Share -- copy and redistribute the material in any medium or format
;            Adapt -- remix, transform, and build upon the material
;
; under the following terms:
;
;      Attribution -- You must give appropriate credit, provide a link to the lic-
;                     ense, and indicate if changes were made. You may do so in any
;                     reasonable manner, but not in any way that suggests the lic-
;                     ensor endorses you or your use.
;    NonCommercial -- You may not use the material for commercial purposes.
;       ShareAlike -- If you remix, transform, or build upon the material, you must
;                     distribute your contributions under the same license as the
;                     original.
;
; The licensor cannot revoke the freedoms above as long as you follow the license
; terms. You may not apply legal terms or technological measures that legally
; restrict others from doing anything the license permits.
;
; For more information on this license, please visit:
; http://creativecommons.org/licenses/by-nc-sa/4.0/
; ---------------------------------------------------------------------------------

; Long-term plan of development:
; --------------------------------------------------------------------------------
; Version Syntactic capability         Tasks
; ------- ---------------------------- -------------------------------------------
;     1.0 Conns and quants             Inference, possibility, consistency, GUI
;     1.5 Temporal/causal models       Probability, modulation
;     2.0 Spatial relations            Command-line interoperability
;     2.5 Deontics, epistemics         Explanations
; --------------------------------------------------------------------------------

; ---------------------------------------------------------------------------------
; Loading code
; ---------------------------------------------------------------------------------

(format t "------------------------------------- Begin loading mReasoner ------------------------------------~%")

(defparameter *system-name* "mReasoner")
(defparameter *version*     "0.9")

(defun file-path (file)
  (let (pathname)
    #+lispworks (setf pathname (current-pathname file))
    #+ccl       (setf pathname (merge-pathnames file (or *load-pathname* *loading-file-source-file*)))
    #+sbcl      (setf pathname (merge-pathnames (sb-unix:posix-getcwd/) file))
    pathname))

#-:jsown (when (probe-file (file-path "jsown-master/load.lisp"))
               (load (file-path "jsown-master/load.lisp")))

#+lispworks (progn 
              ; Get revision information from SVN
              (let ((string-stream (make-string-output-stream)) output position revision-number)
                (system:call-system-showing-output (format nil "cd \"~A\"; svn info" 
                                                           (directory-namestring (current-pathname)))
                                                   :prefix "" :show-cmd nil :output-stream string-stream)
                (setf output (split-sequence (format nil ":MSP~%") (get-output-stream-string string-stream)))
                (setf position (position "Last Changed Rev" output :test #'string-equal))
                (when (numberp position) (setf revision-number (nth (+ 1 position) output)))
                (when revision-number
                  (setf revision-number (parse-integer revision-number :junk-allowed t))
                  (setf *version* (format nil "~A.r~A" *version* revision-number))))

              (setf system:*stack-overflow-behaviour* :warn) ; Suppress stack overflow warnings SSK 2015-02-05

              (defsystem "mReasoner"
                              ()
                              :members ("Classes.lisp"
                                        "Utilities.lisp"
                                        "API.lisp"
                                        "Parser.lisp"
                                        "HighLevel.lisp"
                                        "Scanner.lisp"
                                        "Builder.lisp"       
                                        "FormConclusions.lisp"
                                        "Counterexamples.lisp"
                                        "Diagnostics.lisp"
                                        "jpd-belief-icon.lisp" ;; FIX !!! SSK 4/19/12
                                        "Control.lisp"
                                        "UserInterface.lisp"
                               )
                     :rules  ((:in-order-to :compile :all
                                            (:requires (:load :previous)))))
              
              (lispworks:load-system "mReasoner" :force t))

#+(or ccl mcl openmcl cmu sbcl clisp ecl scl allegro)
(progn
  (load (file-path "Classes.lisp"))
  (load (file-path "Utilities.lisp"))
  (load (file-path "API.lisp"))
  (load (file-path "Parser.lisp"))
  (load (file-path "HighLevel.lisp"))
  (load (file-path "Scanner.lisp"))
  (load (file-path "Builder.lisp"))
  (load (file-path "FormConclusions.lisp"))
  (load (file-path "Counterexamples.lisp"))
  (load (file-path "Diagnostics.lisp"))
  (load (file-path "jpd-belief-icon.lisp"))
  (load (file-path "Control.lisp"))
)

; ---------------------------------------------------------------------------------
; Global variables
; ---------------------------------------------------------------------------------

(defparameter *mR-interface* nil
  "Global variable used to access GUI interface")

(defparameter *exhaustive-search-depth* 7
 "Global variable that controls the combinatoric depth of the exhaustic search
  algorithms defined in Counterexamples.lisp")

(defparameter *stochastic* nil
  "Parameter that enables or disables stochastic system.")

(defparameter *build-attempts* 1000
  "Parameter that establishes the number of attempts to build a stochastic model
   before the system stops trying.")

(defparameter +sigma+ 0.0
  "Parameter that control the execution of system 2 processes (i.e., sigma = search)
   across the system as a whole. When sigma = 0, system 2 processes are never called.
   When sigma = 1, system 2 processes are always called. When sigma = .6, there is a 60%
   chance that system 2 processes will be called.")

(defparameter +lambda+ 4.0
  "Parameter that controls the size of models. Lambda denotes the lambda parameter Poisson
   distribution. By default, lambda = 4. To set a model's size, the system will sample from a
   left-truncated Poisson distribution with lambda parameter = 4. The system will sample from
   the distribution and use that sample as the size (cardinality) of the model.")

(defparameter +epsilon+ 0.0
  "Parameter that controls the construction of canonical models, i.e., models whose individuals
   are specified in the intensions, i.e., epsilon = error. When epsilon = 0, the system always
   builds canonical models. When epsilon = 1, every time the system needs to build a token in a
   model, it will do so with respect to the intensional constraints alone. When epsilon = .6, 
   every time the system needs to build a token in a model, there will be a 60% chance that the
   system will draw from canonical tokens.")

(defparameter +omega+ 1.0
  "Parameter that controls whether individuals weaken their initial conclusion after finding a
   counterexample or whether they simply report 'NVC' once a counterexample is found. When
   omega = 1.0, weakening always occurs; when omega = 0.0, individuals always report 'NVC' when
   a counterexample is found. When omega = 0.6, there's a 60% chance that weakening will occur.")

(defparameter *synthetic-data* nil)

;(defparameter *mailbox* (process-data *standard-output*))

(defun reset-system-parameters ()
  (setf *stochastic* nil)
  (setf +lambda+ 4.0)
  (setf +sigma+ 0.0)
  (setf +epsilon+ 0.0)
  (setf +omega+ 1.0))

(defun stochastic-enabled? ()
  *stochastic*)

(defun generate-size ()
  (let ((size nil)
        (candidate-size nil)
        (left-truncate '(0 1)))
    (while (not size)
      (setf candidate-size (poisson-random-number +lambda+))
      (when (not (member candidate-size left-truncate))
        (setf size candidate-size)))
    size))

(defun build-canonical? ()
  (< (random 1.0) (- 1 +epsilon+)))

(defun system2-enabled? ()
  (< (random 1.0) +sigma+))

(defun weaken-conclusions? ()
  (< (random 1.0) +omega+))

(format t "------------------------------------ Loaded mReasoner ~A -----------------------------------~%~%"
        *version*)
