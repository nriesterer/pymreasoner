(in-package "CL-USER")
(load-all-patches)
(load (current-pathname "+mReasoner.lisp" nil))

; Deliver Mac app with GUI -----------------------------------------------------

#+lispworks (deliver 'start-ui #+:cocoa (create-macos-application-bundle "/Users/skhemlani/Desktop/mReasoner.app")
                     #-:cocoa "mReasoner"
                     0 :interface :capi)
#+lispworks (quit)

; Deliver standalone executable with REPL --------------------------------------
#|
(defun main ()
	(if (> (length (get-command-line-arguments)) 0)
		(let ((input (find-argument "-i"))
		      (output (find-argument "-o")))
                  (write-to-json (read-events-and-execute-query input) output))
          (progn
            (print-header)
            (mreasoner-repl))))

(deliver 'main "mReasoner" 0)
|#

; Deliver standalone executable for SYLLOGISM-CHALLENGE -----------------------

#+ccl (load "~/Documents/MMR\ Lab/Models/Unified/Code/+mReasoner.lisp")

#+ccl (defun syllogism-challenge (syllogism &key (directory "~/") (lambda 3.0) (epsilon 0.45) (sigma 0.5) (omega 1.0) (N 1))
         (initialize-tracer :verbose t)
              (parameter-search
               (list (list (get-syllogistic-premises syllogism)
                           "What follows?" '() T lambda epsilon sigma omega syllogism NIL NIL T))
               :directory directory :N N :verbose nil :parameters (list (list lambda epsilon sigma omega))))

#+ccl (defun main ()
	(if (> (length (get-command-line-arguments)) 0)
            (let ((syllogism (find-argument "-syllogism"))
                  (directory (find-argument "-directory"))
                  (lambda (read-from-string (find-argument "-lambda")))
                  (epsilon (read-from-string (find-argument "-epsilon")))
                  (sigma (read-from-string (find-argument "-sigma")))
                  (omega (read-from-string (find-argument "-omega")))
                  (N (read-from-string (find-argument "-N"))))
             ;(initialize-tracer :verbose nil)
              (format t "~74@<Initializing mReasoner 0.9.6258...~>[DONE]~%")
              (format t "~74@<~A~>[DONE]~%"
                     (format nil "Setting parameters as follows: ~A = ~A, ~A = ~A, ~A = ~A, ~A = ~A" 
                             #\u+03bb lambda #\u+025b epsilon #\u+03c3 sigma #\u+03c9 omega))
             (format t "~74@<~A~>"
                     (format nil "Generating synthetic data for ~A participants..." N))
             (parameter-search
               (list (list (get-syllogistic-premises syllogism)
                           "What follows?" '() T lambda epsilon sigma omega syllogism  NIL NIL T))
               :directory directory :N N :verbose nil :parameters (list (list lambda epsilon sigma omega)))
             (format t "[DONE]~%")
             (format t "~74@<~A~>[DONE]~%"
                     (format nil "Writing data to ~A ..." directory)))
          (progn
            (print-header)
            (mreasoner-repl))))

#+ccl (save-application "~/mReasoner-bin" :toplevel-function #'main :prepend-kernel t)