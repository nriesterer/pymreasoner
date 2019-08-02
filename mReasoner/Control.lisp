(defmacro handling-errors (&body body)
  `(HANDLER-CASE (progn ,@body)
     (simple-condition 
         (ERR) 
       (format *error-output* "~&~A: ~%" (class-name (class-of err)))
       (apply (function format) *error-output*
              (simple-condition-format-control   err)
              (simple-condition-format-arguments err))
       (format *error-output* "~&"))
     (condition 
         (ERR) 
       (format *error-output* "~&~A: ~%  ~S~%"
               (class-name (class-of err)) err))))

(defun mreasoner-repl ()
  (do ((+eof+ (gensym))
       (hist 1 (1+ hist)))
      (nil)
    (format t "~%~A [~D]> " "mReasoner" hist)
    (handling-errors
     (setf +++ ++   ++ +   + -   - (read *standard-input* nil +eof+))
     (when (or (eq - +eof+)
               (member - '((quit)(exit)(continue)) :test (function equal)))
       (return-from mreasoner-repl))
     (setf /// //   // /   / (multiple-value-list (eval -)))
     (setf *** **   ** *   * (first /))
     (format t "~& --> ~{~S~^ ;~%     ~}~%" /))))

(defun get-command-line-arguments ()
  (if (find-package :cl-launch)
    (symbol-value (find-symbol (string :*arguments*) :cl-launch))
    (progn
      #+sbcl (cdr sb-ext:*posix-argv*)
      #+clozure (cdr (ccl::command-line-arguments))
      #+gcl (cdr si:*command-args*)
      #+ecl (loop for i from 1 below (si:argc) collect (si:argv i))
      #+cmu (cdr extensions:*command-line-strings*)
      #+allegro (cdr (sys:command-line-arguments))
      #+lispworks (cdr sys:*line-arguments-list*)
      #+clisp ext:*args*
      #-(or sbcl clozure gcl ecl cmu allegro lispworks clisp)
      (error "get-command-line-arguments not supported for your implementation"))))

(defun run-system-command (command args)
  #+clozure (run-program command args :output *standard-output*))

(defun find-argument (arg)
(let ((argument (member arg (get-command-line-arguments) :test #'string-equal)))
   (when argument (if (> (length argument ) 1) (second argument) argument))))

(defun print-header ()
	(format t "---------------------------------------------------------------~%~
	           mReasoner version ~A.~A (~A)                                     ~%~
                   Binary for Ragni and colleagues' Syllogism Challenge          ~%~
	           Copyright (C) 2017 by S. Khemlani and P.N. Johnson-Laird      ~%~
                                                                                 ~%~
                   Type '(syllogism-challenge \"AA1\")' to run the syllogism     ~%~
                   challenge using the default parameter settings. To modify the ~%~
                   parameter settings, type '(manual)'. Type '(quit)' to quit    ~%~
                   mReasoner.                                                    ~%~
                                                                                 ~%~
                   This software is licensed under a Creative Commons Attribution-~%~
                   NonCommercial-ShareAlike 4.0 International License. For more  ~%~
                   information on this license, please visit:                    ~%~
                                                                                 ~%~
                          http://creativecommons.org/licenses/by-nc-sa/4.0/      ~%~
		   ---------------------------------------------------------------~%"
                *version*
                "6258"
                "2017-09-13"))

(defun get-syllogistic-premises (syllogism)
  (let* ((syllogisms '(("AA1" (list Aab Abc)) ("AA2" (list Aba Acb)) ("AA3" (list Aab Acb)) ("AA4" (list Aba Abc))
                       ("AI1" (list Aab Ibc)) ("AI2" (list Aba Icb)) ("AI3" (list Aab Icb)) ("AI4" (list Aba Ibc))
                       ("AE1" (list Aab Ebc)) ("AE2" (list Aba Ecb)) ("AE3" (list Aab Ecb)) ("AE4" (list Aba Ebc))
                       ("AO1" (list Aab Obc)) ("AO2" (list Aba Ocb)) ("AO3" (list Aab Ocb)) ("AO4" (list Aba Obc))
                       ("IA1" (list Iab Abc)) ("IA2" (list Iba Acb)) ("IA3" (list Iab Acb)) ("IA4" (list Iba Abc))
                       ("II1" (list Iab Ibc)) ("II2" (list Iba Icb)) ("II3" (list Iab Icb)) ("II4" (list Iba Ibc))
                       ("IE1" (list Iab Ebc)) ("IE2" (list Iba Ecb)) ("IE3" (list Iab Ecb)) ("IE4" (list Iba Ebc))
                       ("IO1" (list Iab Obc)) ("IO2" (list Iba Ocb)) ("IO3" (list Iab Ocb)) ("IO4" (list Iba Obc))
                       ("EA1" (list Eab Abc)) ("EA2" (list Eba Acb)) ("EA3" (list Eab Acb)) ("EA4" (list Eba Abc))
                       ("EI1" (list Eab Ibc)) ("EI2" (list Eba Icb)) ("EI3" (list Eab Icb)) ("EI4" (list Eba Ibc))
                       ("EE1" (list Eab Ebc)) ("EE2" (list Eba Ecb)) ("EE3" (list Eab Ecb)) ("EE4" (list Eba Ebc))
                       ("EO1" (list Eab Obc)) ("EO2" (list Eba Ocb)) ("EO3" (list Eab Ocb)) ("EO4" (list Eba Obc))
                       ("OA1" (list Oab Abc)) ("OA2" (list Oba Acb)) ("OA3" (list Oab Acb)) ("OA4" (list Oba Abc))
                       ("OI1" (list Oab Ibc)) ("OI2" (list Oba Icb)) ("OI3" (list Oab Icb)) ("OI4" (list Oba Ibc))
                       ("OE1" (list Oab Ebc)) ("OE2" (list Oba Ecb)) ("OE3" (list Oab Ecb)) ("OE4" (list Oba Ebc))
                       ("OO1" (list Oab Obc)) ("OO2" (list Oba Ocb)) ("OO3" (list Oab Ocb)) ("OO4" (list Oba Obc))))
         (syllogistic-premises (find syllogism syllogisms :test #'(lambda (x y) (equal x (first y))))))
    (if syllogistic-premises
      (eval (second syllogistic-premises))
      (error "Improperly formatted syllogism code"))))

#|(defun manual ()
   (sys:open-url "http://www.google.com/"))|#

(defclass event ()
  ((name              :accessor name              :initarg :name)
   (directly-after    :accessor directly-after    :initarg :after)
   (starts-with       :accessor starts-with       :initarg :start)
   (simultaneous-with :accessor simultaneous-with :initarg :simultaneous)
   (ends-with         :accessor ends-with         :initarg :end))
  (:documentation "Event class"))

(defclass actre-event (event)
  ((act               :accessor act               :initarg :act)
   (with              :accessor with              :initarg :with)
   (event-type        :accessor event-type        :initarg :type)
   (where             :accessor where             :initarg :where)
   (time              :accessor event-time        :initarg :time))
  (:documentation "ACT-R/E event class"))

(defun get-json-attribute-value (attribute json)
  (dolist (x json)
    (if (and (equal (type-of x) 'cons) (string-equal attribute (car x))) (return (cdr x)))))

(defun actre-query-now (events)
  (let* ((positions (loop
                    for event in events 
                    and position from 0
                    when (string-equal (event-type event) "newTopLevel")
                    collect event))
         (positions (reverse positions))
         response)
    (setf response (list :obj (list "response" :obj
                                    (cons "act" (act (first positions)))
                                    (cons "for" (with (second positions)))
                                    (cons "where" (where (second positions))))))

    response))

(defun actre-query-who-all (events)
  (let ((positions (loop
                    for event in events 
                    and position from 0
                    when (and (with event) (string-equal (act event) "assist-goal"))
                    collect event))
        response)

    (if positions
        (setf response (list :obj (list "response" :obj
                                        (cons "answer" "true")
                                        (append (list "helped")
                                                (mapcar #'(lambda (x y) (list :obj (cons "who" x) (cons "where" y)))
                                                        (mapcar #'with positions) (mapcar #'where positions))))))
      (setf response (list :obj (list "response" :obj
                                            (cons "answer" "false")
                                            (list "helped")))))
    response))

(defun actre-query-with-person (person events)
  (let ((positions (loop
                    for event in events 
                    and position from 0
                    when (string-equal person (with event))
                    collect event))
        response)
    (if positions
        (setf response (list :obj (list "response" :obj
                                        (cons "answer" "true")
                                        (cons "who" person)
                                        (cons "where" (where (first positions))))))
      (setf response (list :obj (list "response" :obj
                                      (cons "answer" "false")
                                      (cons "who" person)
                                      (cons "where" "null")))))
    response))

(defun make-actre-event (json)
  (make-instance 'actre-event
                 :name         (first json)
                 :after        (get-json-attribute-value "preceding" json)
                 :start        (get-json-attribute-value "start" json)
                 :simultaneous (get-json-attribute-value "simultaneous" json)
                 :end          (get-json-attribute-value "end" json)
                 :where        (get-json-attribute-value "where" json)
                 :time         (get-json-attribute-value "time" json)
                 :type         (get-json-attribute-value "type" json)
                 :with         (get-json-attribute-value "with" json)
                 :act          (get-json-attribute-value "act" json)))

(defun read-events-and-execute-query (input)
  (let* ((json (read-from-json input))
         (query (get-json-attribute-value "query" json))
         (query-type (get-json-attribute-value "type" query))
         (query-ref  (get-json-attribute-value "reference" query))
         (events (remove-if #'(lambda (x)
                                (or (not (listp x))
                                    (string-equal (first x) "query"))) json))
         (events (mapcar #'make-actre-event events)))
    (handling-errors
      (cond
       ((string-equal query-type "now")     (actre-query-now events))
       ((string-equal query-type "who-all") (actre-query-who-all events))
       ((string-equal query-type "with")    (actre-query-with-person query-ref events))
       (t                                   (error "Unknown query"))))))

(defun read-from-json (input)
  (let ((in (open (file-path input) :if-does-not-exist nil))
        (json " "))
    (when in
      (loop for line = (read-line in nil)
            while line do 
            (progn
              (setf json (concatenate 'string json (format nil "~A~%" line)))))
      (close in))
    (jsown:parse json)))

(defun write-to-json (json output)
  (with-open-file (out output :direction :output :if-exists :supersede)
    (format out (jsown:to-json json))))
