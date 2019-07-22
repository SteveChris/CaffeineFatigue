;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Authors     : Dan Bothell, Rick Moore, Bella Veksler
;;; Copyright   : (c) 2016 Cognitive Models and Agents Branch
;;; Availability: Covered by the GNU LGPL, see LGPL.txt
;;; Address     : Cognitive Models and Agents Branch
;;;             : Air Force Research Laboratory
;;;             : 2620 Q Street, Bldg 852, Rm 3-312
;;;             : Wright-Patterson AFB, OH  45433-7905
;;;             : bellav717@gmail.com
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Filename    : fatigue.lisp
;;; Version     : 3.2
;;; 
;;; Description : Dampens activations based on a fatigue procedural (fp) parameter.
;;;               The rate of decay is governed by fp-dec, and a stimulus parameter
;;;               restores fp to a percentage of its original state. Corresponding 
;;;               fd parameter exists for the declarative component. Uses both a
;;;               biomathematical model and a time-on-task component to attenuate 
;;;               utility values.
;;; 
;;; Bugs        : 
;;;
;;; To do       : 
;;; 
;;; ----- History -----
;;; 2009.01.23 Dan
;;;             : * Initial conversion of the run-for-x function into something
;;;             :   that works off of the conflict-set-hook and is encapsulated
;;;             :   in a module.
;;; 2009.02.17 Rick Moore
;;;             : * Added :stimulus parameter.
;;; 2009.02.27 Dan
;;;             : * Modified things so that it checks the delayed-resolution
;;;             :   status of the procedural before deciding whether or not to
;;;             :   actually decrement.
;;;             : * Also added code to deal with the fact that when the threaded
;;;             :   cognition code is also active there can be multiple calls to
;;;             :   the conflict-set-hook "simultaneously" and it's only really
;;;             :   the last one that matters.
;;; 2009.03.09 Rick Moore
;;;             : * Adeded :ut as an observed parameter to avoid (sgp) calls.
;;; 2009.03.17 Rick Moore
;;;             : * Only compare ut if ut is not nil.
;;; 2009.08.19 Tim Halverson
;;;             : * Added fd and fd-dec parameters, along with supporting code,
;;;                 to allow the fatigue module to affect retrievals.
;;;             : * Added initializers to original-fp and original-fd in the
;;;                 defstruct of fatigue-module. This is required because
;;;                 the :stimuli parameter requires these values to be non-nil
;;;                 and the order in which the parameters get initialized when 
;;;                 the module is instantiated is not defined. Therefore, 
;;;                 depending on the implementation of hash tables in the version
;;;                 of lisp used, original-fp and/or original-fd could be nil
;;;                 when :stimuli is initialized.
;;; 2011.04.19 Rick Moore
;;;             : * Added parameters for utbmc, utmc, utc, fpbmc, fpmc, fpc, and
;;;                 hour Also added calculations for within-task
;;;                 fatigue effects.
;;; 2011.05.13 Rick Moore
;;;             : * Removed original-fp, added fp-percent, and changed fp-dec
;;;                 so it now decrements fp-percent.  Stimulus is simply
;;;                 assigned to fp-percent.
;;; 2011.11.20 Clayton Stanley
;;;             : * Added ability to define a biomath-class, and use it in the fatigue module
;;;		              An implementation for the McCauley 2009 biomath model is included here
;;;                 And set as the default biomath-class to use
;;; 2012.08.05 Rick Moore
;;;             : * Linked fd to biomathematical model of fatigue
;;;             : * Individual mechanisms can now be turned on or off with fp = nil 
;;;                 or not nil, and fd = nil or not nil.
;;;             : * Setting fp or fd does not do anything any more, but they can be 
;;;                 read to find the value that the module last used
;;;             : * Resetting the module causes its own defaults to be used-- not sure 
;;;                 why-- but aligned those with correct defaults.
;;;             : * Set default value for fp-dec.
;;;             : * Fixed more default settings
;;;             : * Updated declarative mechanisms to use biomathematical model
;;;                 calculations
;;;             : * Added simple optimization (a cache) to biomath calculations
;;; 2012.09.27 Rick Moore
;;;             : * Added run-with-microlapses function and removed 
;;;                 conflict-set-hook
;;; 2012.11.06 Rick Moore
;;;             : * Fixed a bug where rounding issues could cause an infinite loop
;;;                 in run-with-microlapses
;;; 2012.11.14 Bella Veksler
;;;             : * Added calculations for initial values of pn and un based 
;;;                 on :tib (time in bed) and :tn (time awake).
;;; 2012.11.19 Bella Veksler
;;;             : * Added set-sleep-history function which modifies pn,un,qn, and vn
;;;                 based on a sleep history of the form '((w1 s1)(w2 s2)...) such that
;;;                 wn is the time awake during day n and sn is time asleep that night
;;; 2015.1.22 Bella Veksler
;;;		: * Changed utc parameter to ut0, this will now be the initial utility
;;;	        	threshold value
;;;		: * Implemented new version of fatigue module where the fp and ut 
;;;			calculations are done using the formula: fp=fpbmc*bp+min^fpmc
;;; 2015.1.29 Bella Veksler
;;;             : * McCauley 2013 implementation of fatigue model incorporated into module
;;; 2015.3.13 Bella Veksler
;;;             : * Fatigue module uses new equation relating biomath and TOT to FP and UT
;;; 2016.5.13 Bella Veksler
;;;             : * Multiplicative FP-Dec mechanism instead of subtractive
;;; 2016.8.18 Bella Veksler
;;;             : * Automatic schedule set to wake at 0730 and go to sleep at 22:00 at model reset
;;; 2016.09.22 Dan Bothell
;;;             : * All of my changes noted here are taged with a comment that 
;;;             :   start with "Dan:".
;;;             : * Addressed some issues that I feel are necessary before making
;;;             :   it generally available through extras using the simplest approach
;;;             :   I could think of -- different approaches could be used to address
;;;             :   these issues:
;;;             :  - Don't change *read-default-float-format* globally and instead
;;;             :    just marked every float here as being a double (probably not
;;;             :    really necessary especially since some are being converted to
;;;             :    rationals anyway -- why not just specify the rational directly?).
;;;             :  - Removed the class-slots-to-params-list function because it
;;;             :    doesn't properly provide those items as parameters -- it
;;;             :    requires that they be set to lists, but they need to be 
;;;             :    numbers.  To go along with that the use of that is then
;;;             :    removed from the module definition and the parameter function
;;;             :    doesn't need to handle them.  That also has the benefit of
;;;             :    removing the need for the MOP which means it should run in 
;;;             :    any ANSI CL instead of only those with MOP support that are 
;;;             :    explicitly tagged in the code around that definition.
;;;             :  - Changed the order of the setfs in the params function for the :hour
;;;             :    parameter so that the value set is returned as the current value.
;;;             :  - Added the provide at the end to support the use of require-extra.
;;;             : * Also added some comments in other places with things that I
;;;             :   think should be addressed, but aren't absolutely necessary.
;;;             : * Commented out unnecessary code in the reset function.
;;; 2017.04.13 Dan Bothell
;;;             : * Moved the hook function settings from the reset function to
;;;             :   the parameters function to ensure they're set when the 
;;;             :   mechanism is enabled, and similarly, remove them when it's
;;;             :   disabled to avoid issues in either direction with other
;;;             :   extensions.
;;;             : * Have the module monitor :egs so it doesn't need to call sgp
;;;             :   for the utility calculations since sgp is slow and that's
;;;             :   a frequent operation (between this and using the previously
;;;             :   recorded :dat value shaved about 4% off the run time of
;;;             :   the demo model).
;;;             : * Similarly, use with-parameters instead of sgp where possible
;;;             :   for performance improvement and simpler code.
;;;             : * Added the monitor for :conflict-set-hook and change the
;;;             :   testing in the parameters function because that hook can
;;;             :   have multiple values set and as long as fatigue's function
;;;             :   is among them things are fine.  That's important because the
;;;             :   ACT-R Environment uses that hook and otherwise fatigue would
;;;             :   not work with the Environment.
;;;             : * Changed the warnings from being output with warning to
;;;             :   print-warning because the former is suppressed by :v nil and
;;;             :   those seem like they're important enough that they should 
;;;             :   always be displayed.
;;;             : * Added a parameter to allow check-for-fatigue-dec to not microlapse
;;;             :   if there aren't any matching productions because all that does
;;;             :   is run the model in a "never-ending" loop of microlapses that
;;;             :   will drive everything way down as far as I can tell.  That 
;;;             :   parameter is called :lapse-on-empty.
;;;             :   Since the modeler is just going to have to set the :stimulus 
;;;             :   after that anyway, why not provide an option for not going through 
;;;             :   the effort of executing them?  In fact, not microlapsing on an empty 
;;;             :   conflict set does not affect the results of the PVTconflictsethook 
;;;             :   model at all!  So, defaulting to off seems like the right answer for
;;;             :   that because it also makes it much easier to take an existing
;;;             :   model/experiment and just turn on fatigue to see what happens.
;;;             : * Fixed a bug in the fatigue-bla-hook function because it was
;;;             :   always using the starting hour instead of updating for the
;;;             :   current time. (fix based on the corresponding call in the
;;;             :   utility hook function).
;;;             : * Changed the events scheduled for check-for-dec-now and 
;;;             :   decrement-fp-fd to credit them as fatigue module actions instead
;;;             :   of procedural.
;;; 2017.04.19 Dan Bothell
;;;             :   ### This change affects the results from the PVTconflicsethook model
;;;             :   ### test run which has a fixed :seed because the first wait production
;;;             :   ### firing on the first trial falls below threshold which
;;;             :   ### changes the first response time and alters the sequence of
;;;             :   ### random numbers from that point on. I don't think it significantly
;;;             :   ### affects the real results, but I don't know for sure because I
;;;             :   ### don't know how to interpret the output.
;;;             :   ### Regardless of that however, it is a bug and needs to be fixed.
;;;             : * When the mechanism is enabled it has to set :ut to the ut0 value
;;;             :   otherwise the first conflict-resolution action doesn't have
;;;             :   a threshold when it picks a production.  That is being done
;;;             :   by scheduling an event to prevent/avoid any interference with
;;;             :   other modules or task code that may also be changing things.
;;;             : * Changed the d** functions for computing the biomath table
;;;             :   to not keep track of the whole list in the all variable because
;;;             :   that didn't seem to be used and the appends (which require a
;;;             :   copy each time) actually caused a significant performance hit.
;;;             :   This sped up the PVT task by ~2% without changing the results.
;;; 2017.05.01 Bella Veksler
;;;             : * Set Schedule now sets the initial hour to the first hour in the schedule
;;;             : * If modeler tries to set the hour parameter to something outside the schedule,
;;;             :   a warning is printed and the fatigue module is turned off
;;; 2017.05.02 Bella Veksler
;;;             : * reset-stimulus function added to allow modeler to indirectly 
;;;             :   reset the stimulus parameter to 1 (i.e. alerting the model)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; General Docs:
;;; 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Public API:
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Design Choices:
;;; 
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; The code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+:packaged-actr (in-package :act-r)
#+(and :clean-actr (not :packaged-actr) :ALLEGRO-IDE) (in-package :cg-user)
#-(or (not :clean-actr) :packaged-actr :ALLEGRO-IDE) (in-package :cl-user)

(defstruct fatigue-module 
  enabled            ; Turns fatigue module off / on
  stimulus           ; Signals a reset to fp based on specified scalar.  fp <- stimulus.  Resets decrements
  fp-dec             ; Decrease in fp after each microlapse
  (fp 1.0)           ; "fatigue parameter" value scales utility calculations
  fd-dec 
  (fd 1.0) 
  ut                 ; Utility threshold (from utilty module)
  dat                ; Dat from whatever module
  egs                ; Dan record this from utility module too
  fpbmc              ; Coefficient relating biomath value to fp
  fpmc               ; Coefficient relating minute within session to fp
  fdbmc              ; Coefficient relating biomath value to fd
  fdc                ; Constant fd offset
  utbmc              ; Coefficient relating biomath value to ut
  utmc               ; Coefficient relating minute within session to ut
  ut0                ; Constant ut offset
  (fp-percent 1.0)   ; The current percentage of fp after microlapses
  (fd-percent 1.0)   ; The current percentage of fp after microlapses
  hour               ; Initiates a new session by providing the number of hours since the beginning of the sleep schedule
  start-time         ; Records the (mp-time) at the start of a session, so proper within-session offsets can be calculated

  pending 
  last-one-empty 
  bl-hook
  utility-hook
  last-biomathematical-hour     ;To optimize biomath calculations- last calculated hour 
  last-biomathematical-value    ;To optimize biomath calculations- last calculated value for last-biomathematical-hour
  biomath-class-obj             ;Stores current instantiation of the chosen boimath class
  
  ;; Dan - flag to not microlapse on empty conflict-set
  lapse-on-empty
  )


#|
Biomathematal model section. 

In order to use a biomath model within the fatigue module, a CLOS class is created for the biomath model. 
All slots in that class should be parameters in the biomath model. 

Documentation for the slots in that class will be used as documentation for these additional parameters in the fatigue module. 

All slots for the class will be exposed to the modeler, and he/she can modify them away from defaults, using the standard sgp interface

A single method must be defined for the class, compute-biomath-value, that is given the time (in hours), and returns the human biomathematical performance

The compute-biomath-value method relies on (mp-time) to adjust the performance value with respect to the time in hours

Set the dynamic variable *biomath-class* to the name of the class being used (as a symbol) in order to load this particular class for the fatigue module.
|#

;Dynamic variable that stores the symbol for the biomath class being used
;If you want to use another class, you can either change this variable here in the code (after defining the new class in the code here)
;Or, you can define the class in another file that gets loaded, then change the value of this variable, then unload and load the fatigue module


;;; Dan: this setting should probably also be part of the module struct so that multiple
;;; models can have separate settings, and could be provided as a parameter in the
;;; module or set with a function like feature sets are for the visual encoding using
;;; the set-char-feature-set function.

(defvar *biomath-class* 'biomath-model-mccauley-simplified)


(defmacro defclass-default (class-name superclasses slots &rest class-options)
  "Shorthand defclass syntax; structure similar to defclass
  Pass three values: slot-name, :initform, and :documentation
  Everything else gets filled in to standard defaults"
  `(defclass 
     ,class-name 
     ,superclasses 
     ,(mapcar (lambda (x) `( ,(first x)
                             :accessor ,(first x)
                             :initarg ,(intern (symbol-name (first x)) "KEYWORD")
                             :initform ,(second x)
                             :documentation ,(third x)))
              slots)
     ,@class-options))

(defclass-default biomath-model-mccauley-simplified ()
                  ((alphaw 	(rational -0.028d0)	"alphaw doc")
                   (alphas 	(rational -0.26d0)	 	"alphas doc")
                   (betaw 	(rational -0.26d0)	 	"betaw doc")
                   (betas 	(rational -0.26d0)	 	"betas doc")
                   (muw 	        (rational 0.33d0)	 	"muw doc")
                   (mus 	        (rational -1.5d0)	 	"mus doc")
                   (etaw 	(rational 0.0074d0) 	"etaw doc")
                   (etas 	(* (/ (/ (rational 20.2d0) 24) (- (/ (rational 20.2d0) 24) 1)) (rational 0.0074d0))	"etas doc")
                   (tau 	        24		        "tau doc")
                   (fi1 	        (rational 21.2d0)	 	"fi1 doc")
                   (Wc 	        (rational 20.2d0)		"Wc doc")
                   (Ac    	(/ (rational 20.2d0) 24) 	"Ac doc")
                   (lambdas 	(rational -0.49d0) 	"lambdas doc")
                   (lambdaw	(rational 0.49d0)		"lambdaw doc") 
                   (ksi		(rational 1.09d0)		"ksi doc") 
                   (W		16		        "W doc")
                   (precision	(/ .05 3)		"precision doc")
                   (table        nil                    "hash-table with all biomath performance values")))

;I had to initialize the object before registering the fatigue module
;Apparently, ACT-R tries to run sgp before calling a reset of the fatigue module.
;Inside reset is where this object gets initialized, but since it isn't called before sgp,
;I'm pretty sure we have to initialize it here - CS

(defun write-list-to-file (l filename)
  "Write a list to a file"
  (with-open-file (fileout filename :direction :output :if-exists :append :if-does-not-exist :create)
    (loop for i to (- (length l) 2) do
          (format fileout "~10f~c" (nth i l) #\tab))
    (format fileout "~10f ~c~%" (car (last l)) #\tab)))


;;====================These are helper functions for the McCauley 2013 fatigue model
(defun merge-hash-tables (&rest tables)
  "returns the union of two or more hash tables"
  (let ((union
         (make-hash-table
          :test (first
                 (sort (mapcar #'hash-table-test tables) #'>
                       :key (lambda (test)
                              (ecase test
                                (eq 0)
                                (eql 1)
                                (equal 2)
                                (equalp 3)))))
          :size (reduce #'max (mapcar #'hash-table-size tables)))))
    (dolist (table tables)
      (maphash (lambda (key val) (setf (gethash key union) val)) table))
    union))

(defun ordered-keys (table)
"given a hash table, returns a list of ordered keys, assumes all keys are numeric"
  (sort
   (loop for key being each hash-key of table
         collect key)
   #'<)) 

(defun print-hash-entry (key value filename)
  "given a key and value pair, writes them out to a file"
  (if (listp value)
    (write-list-to-file (list key (first value) (second value) (third value) (fourth value)) filename)
    (write-list-to-file (list key value) filename))
)

(defun print-hash-table-ordered (table stream)
  "given a hash table, writes it out ordered by key value (assumed numeric) to a file"
  (let ((stream (open stream :if-exists :supersede :direction :output))
        (orderedkeys (ordered-keys table)))
    (write-list-to-file '("hour" "alertness" "u" "k" "awake") stream)
    (close stream)
    (mapcar #'(lambda(x) (print-hash-entry x (gethash x table) stream)) orderedkeys))
  nil)

(defun print-hash-table-ordered-listener (table)
  "given a hash table, writes it out ordered by key value (assumed numeric) to the listener"
  (let ((orderedkeys (ordered-keys table)))
    (mapcar #'(lambda(k) (format t (concatenate 'string (write-to-string k) ": " (write-to-string (gethash k table)) "~%"))) orderedkeys))
  nil)

(defun print-hash-table (table)
  "given a hash table, writes it out unordered to the listener"
  (let ((keys (loop for key being each hash-key of table collect key)))
    (mapcar #'(lambda(k) (format t (concatenate 'string (write-to-string k) ": " (write-to-string (gethash k table)) "~%"))) keys))
  nil)

(defun get-last-value (table)
  "returns the last value in a hash table, assumes keys are numeric"
   (let ((orderedkeys (ordered-keys table)))
     (gethash (car (last orderedkeys)) table))
)

(defun get-last-key (table)
  "returns the last key in a hash table, assumes keys are numeric"
   (let ((orderedkeys (ordered-keys table)))
     (car (last orderedkeys)))
)

(defun positions (number list)
  "Returns the positions of its first arg in second arg."
  (let ((position ()))
    (loop until (null (member number list))
          do (setf position (position number list))
          collect position
          do (setf list  (substitute 'x number list 
                                     :end (1+ position))))))
(defun my-sort (function lists)
  "Non-destructive sort."
  (loop for sorted-item in 
        (sort (loop for item in lists
                    collect (list item)) function :key #'car)
        collect (first sorted-item)))

(defun find-closest (number list)
  "Finds the closest number in list."
  (let ((test (loop for item in list
                    collect (abs (- number item)))))
    (nth (car (positions (first (my-sort '< test)) test)) list)))

;=============================================================================
#| Begin biomathematical section |#
;;;The following are all part of the implementation of the functions in 
;;;    McCauley, P., Kalachev, L. V, Mollicone, D. J., Banks, S., Dinges, D. F., & Van Dongen, H. P. a. (2013). 
;;;    Dynamic circadian modulation in a biomathematical model for the effects of sleep and sleep loss on 
;;;    waking neurobehavioral performance. Sleep, 36(12), 1987–97.
;;;

;Equation 7a
(defmethod kappaw ((obj biomath-model-mccauley-simplified) t1 t0 k0)
  (with-slots (lambdaw precision) obj
    (let ((sofar k0)
          (my-hash (make-hash-table)))
      (setf (gethash t0 my-hash) sofar)
      (while (< t0 t1)
             (setf sofar (+ sofar (* precision lambdaw sofar (- 1 sofar))))
             (setf t0 (+ t0 precision))
             (setf (gethash t0 my-hash) sofar)
             )
      my-hash)))

;Equation 7b
(defmethod kappas ((obj biomath-model-mccauley-simplified) t1 t0 k0)
  (with-slots (lambdas precision) obj
    (let ((sofar k0)
          (my-hash (make-hash-table)))
      (setf (gethash t0 my-hash) sofar)
      (while (< t0 t1)
             (setf sofar (+ sofar (* precision lambdas sofar)))
             (setf t0 (+ t0 precision))
             (setf (gethash t0 my-hash) sofar)
             )
      my-hash)))

;Equation 6
(defmethod ct ((obj biomath-model-mccauley-simplified) ti)
  (with-slots (tau fi1) obj
    (sin (* (* 2 pi) (/ (- ti fi1) tau)))
    ))

;Equation 5a
(defmethod gw ((obj biomath-model-mccauley-simplified) t1 t0 k0)
  (with-slots (ksi muw) obj
    (let ((dk (kappaw obj t1 t0 k0))
          (my-hash (make-hash-table)))
      (setf (gethash t0 my-hash) k0)
      (maphash #'(lambda (key value) 
                   (setf (gethash key my-hash) (* ksi (+ value) (+ muw (ct obj key))))) dk)
      my-hash)))

;Equation 5b
(defmethod gs ((obj biomath-model-mccauley-simplified) t1 t0 k0)
  (with-slots (alphas betas etas mus) obj
    (let ((dk (kappas obj t1 t0 k0))
          (my-hash (make-hash-table)))
      (setf (gethash t0 my-hash) k0)
      (maphash #'(lambda (key value) 
                   (setf (gethash key my-hash) (+ (/ (* alphas betas) etas) (* value (+ mus (ct obj key)))))) dk)
      my-hash)))

;Equation 3a
(defmethod duw ((obj biomath-model-mccauley-simplified) t1 t0 u0)
  (with-slots (etaw precision) obj
    (let* ((sofar u0)
           (my-hash (make-hash-table)))
      (setf (gethash t0 my-hash) sofar)
      (while (< t0 t1)
             (setf sofar (+ sofar (* etaw sofar precision)))
             (setf t0 (+ t0 precision))
             (setf (gethash t0 my-hash) sofar)
        )
      my-hash)))

;Equation 3b
(defmethod dus ((obj biomath-model-mccauley-simplified) t1 t0 u0)
  (with-slots (etas precision) obj
    (let* ((sofar u0)
           (my-hash (make-hash-table)))
      (setf (gethash t0 my-hash) sofar)
      (while (< t0 t1)
             (setf sofar (+ sofar (* (+ 1 (* etas sofar)) precision)))
             (setf t0 (+ t0 precision))
             (setf (gethash t0 my-hash) sofar)
        )
      my-hash)))


;Equation 2a
(defmethod dpw ((obj biomath-model-mccauley-simplified) t1 t0 p0 u0 k0)
  (with-slots (alphaw betaw precision) obj
    (let* ((sofar p0)
           (u (duw obj t1 t0 u0))
           (g (gw obj t1 t0 k0))
           (dk (kappaw obj t1 t0 k0))
           (my-hash (make-hash-table)))
      (setf (gethash t0 my-hash) (list p0 u0 k0 1))
      (while (< t0 t1)
             (setf sofar (+ sofar (* precision (+ (* alphaw (+ sofar (* betaw (gethash t0 u)))) (gethash t0 g)))))
             (setf t0 (+ t0 precision))
             (setf (gethash t0 my-hash) (list sofar (gethash t0 u) (gethash t0 dk) 1))
        )
      my-hash)))

;Equation 2b
(defmethod dps ((obj biomath-model-mccauley-simplified) t1 t0 p0 u0 k0)
  (with-slots (alphas betas precision) obj
    (let* ((sofar p0)
           (u (dus obj t1 t0 u0))
           (g (gs obj t1 t0 k0))
           (dk (kappas obj t1 t0 k0))
           (my-hash (make-hash-table)))
      (setf (gethash t0 my-hash) (list p0 u0 k0 0))
      (while (< t0 t1)
             (setf sofar (+ sofar (* precision (+ (* alphas (+ sofar (* betas (gethash t0 u)))) (gethash t0 g)))))
             (setf t0 (+ t0 precision))
             (setf (gethash t0 my-hash) (list sofar (gethash t0 u) (gethash t0 dk) 0))
        )
      my-hash)))
#| End of biomathematical section  |#
;=========================================================================
;;for testing purposes, these are the schedules from Hans Van Dongen's sleep studies and can be compared to McCauley et al data
;;control: (print-hash-table-ordered (set-schedule '((7.5 23.5)(31.5 47.5)(55.5 71.5)(79.5 94.5))) (merge-pathnames "../actrschedcontrol.txt"))))
;;deprivation (62 hr): (print-hash-table-ordered (set-schedule '((8 22)(32 46)(56 118)(128 142))) (merge-pathnames "../actrscheddep.txt"))
;;restriction (4 hr sleep for 13 days): (print-hash-table-ordered (set-schedule '((7.5 23.5)(31.5 47.5)(55.5 71.5)(79.5 99.5)(103.5 123.5)(127.5 147.5)(151.5 171.5)(175.5 195.5)(199.5 219.5)(223.5 243.5)(247.5 267.5)(271.5 291.5)(295.5 315.5)(319.5 339.5)(343.5 363.5)(367.5 387.5)(391.5 407.5))) (merge-pathnames "../actrschedrest.txt"))

;The following is a function to set the sleep/wake schedule of the individual
;given a schedule of sleep/wake hours in the form of '((awake1 sleep1) (awake2 sleep2)....) 
;returns a hashtable with all performance values where keys are the time 
(defun set-schedule (sched &key (p0 3.841432) (u0 38.509212) (k0 0.019455))
  "sets the sleep-wake schedule"
  (let ((current (list p0 u0 k0))
        (my-hash (make-hash-table))
        (today)
        (tonight)
        (module (get-module :fatigue)))
    (setf (fatigue-module-last-biomathematical-hour module) (first (first sched))) 
    (setf (fatigue-module-last-biomathematical-value module) p0)
    (dotimes (s (length sched))
      (setf today (dpw (fatigue-module-biomath-class-obj module) (rational (second (nth s sched))) (rational (first (nth s sched))) (first current) (second current) (third current)))
      (setf current (get-last-value today))
      (remhash (get-last-key today) today)
      (setf my-hash (merge-hash-tables my-hash today))
      (if (< s (1- (length sched)))
        (progn
          (setf tonight (dps (fatigue-module-biomath-class-obj module) (rational (first (nth (1+ s) sched))) (rational (second (nth s sched))) (first current) (second current) (third current)))
          (setf current (get-last-value tonight))
          (remhash (get-last-key tonight) tonight)
          (setf my-hash (merge-hash-tables my-hash tonight))
          )))
    (setf (table (fatigue-module-biomath-class-obj module)) my-hash)
    (setf (fatigue-module-hour module) (first (first sched))) ;BZV - set to first hour in schedule as default
    my-hash))

(defun reset-stimulus (&optional (stimulus 1))
  "resets the stimulus parameter in module"
  (if (numberp stimulus)
      (sgp-fct (list :stimulus stimulus))
      (print-warning "Stimulus value must be a number instead of ~s" stimulus)))

(defmethod compute-biomath-value ((obj biomath-model-mccauley-simplified) hour)
  "Looks up the performance value in a hash table which was created using the McCauley et al. (2013) paper"
  (with-slots (table) obj
      (car (gethash (find-closest hour (ordered-keys table)) table))))

(defun compute-biomath-value-for-hour( hour )
  "Returns either the last computed biomath value or recomputes for new hour if too long ago"
	(let ((module (get-module :fatigue)))	
		(if (< (abs (- hour (fatigue-module-last-biomathematical-hour module))) (/ 1 60))
    		(fatigue-module-last-biomathematical-value module)
    	(progn
      		(setf (fatigue-module-last-biomathematical-hour module) hour)
     		 (setf (fatigue-module-last-biomathematical-value module) 
           		 (compute-biomath-value (fatigue-module-biomath-class-obj module) hour))))))
			
(defun run-until-time-or-condition (time condition &key (real-time nil))
  "allows for running the model either until time or condition has been met"
  (let* ((end-time (+ (mp-time) time))
         (end-now nil)
         (end-event (schedule-event end-time (lambda ()(setf end-now t)) :maintenance t :priority :min :output nil)))
    (run-until-condition (lambda ()
                           (cond (end-now t) ;; time limit reached
                                 ((funcall condition) ;; condition met
                                  (delete-event end-event) ;; remove the end-time event
                                  t)))
                         :real-time real-time)))

(defun decrement-fp-fd ()
  "Anytime there is a microlapse, the fp-percent and fd-percent are decremented"
  (let ((module (get-module :fatigue)))
    (setf (fatigue-module-fp-percent module) (* (fatigue-module-fp-percent module) (fatigue-module-fp-dec module)))
    (setf (fatigue-module-fd-percent module) (* (fatigue-module-fd-percent module) (fatigue-module-fd-dec module)))
    ;BZV  (format t "decrementing ~s ~%"(fatigue-module-fp-percent module))
    ))

(defun check-for-dec-now ()
  "schedules decrement of fp and fd"
  (let ((module (get-module :fatigue))
        (p (get-module procedural)))
    (setf (fatigue-module-pending module) nil)
    (when (and (fatigue-module-enabled module)
               (procedural-delayed-resolution p)
               (fatigue-module-last-one-empty module)
               (find (procedural-delayed-resolution p) (meta-p-delayed (current-mp))))
      (schedule-event-relative (randomize-time (fatigue-module-dat module))
                               'decrement-fp-fd
                               :module :fatigue 
                               ))))

(defun check-for-fatigue-dec (conflict-set)
  "conflict-set hook to determine if decrement should be scheduled due to microlapse"
  (let ((module (get-module :fatigue)))
    ;;check whether need to have a microlapse (either empty set or all productions below threshold)
    (when (and (fatigue-module-enabled module)    
               (or (and (fatigue-module-lapse-on-empty module) (null conflict-set))
                   (and conflict-set      
                        (and (fatigue-module-ut module) (< (production-utility (car conflict-set)) (fatigue-module-ut module)))))
               (not (fatigue-module-pending module)))      
      (setf (fatigue-module-pending module) t)
      (schedule-event-relative 0  'check-for-dec-now
                               :module :fatigue 
                               :maintenance t))
    ;;catch empty set or all below threshold
    (setf (fatigue-module-last-one-empty module)
          (or (null conflict-set)
              (and (fatigue-module-ut module) (< (production-utility (car conflict-set)) (fatigue-module-ut module))))))
  nil)

(defun fatigue-utility-hook ( production )
  "Procedural hook: Applies a fatigue function to the utility and utility threshold calculation."
  (let ((module (get-module :fatigue))
        (fp)
        (ut)
        (minutes-passed)
        (biomath-prediction)
        (finalutility))
    (if (and (fatigue-module-enabled module) (fatigue-module-fp module))
      (let ((alert-utility)
            (egs (fatigue-module-egs module)))
        
        ;Calculate alert utility value (w/o fatigue turned on)
        (setf (fatigue-module-enabled module) nil)
        (with-parameters (:egs 0)
          (setf alert-utility (compute-utility production))
          )
        (setf (fatigue-module-enabled module) t)
        
        ; Calculate the biomathematical model prediction
        (setf biomath-prediction (compute-biomath-value-for-hour (+ (/ (mp-time) 3600) (fatigue-module-hour module))))   ; BZV hour to the minute that we want biomath value

        ; Calculate the number of minutes that have passed
        (setf minutes-passed (max 0 (/ (- (mp-time) (fatigue-module-start-time module)) 60)))

        ; Calculate ut
        (setf ut  (* (- 1 (* biomath-prediction (fatigue-module-utbmc module))) 
                   (expt (+ 1 minutes-passed) (fatigue-module-utmc module))))    
        (setf ut (* (fatigue-module-ut0 module) ut))
        (sgp-fct (list :ut ut))

        ; Calculate the value for fp
        (setf fp (* (- 1 (* biomath-prediction (fatigue-module-fpbmc module))) 
                   (expt (+ 1 minutes-passed) (fatigue-module-fpmc module))))     

        ; Add in microlapses
        (setf fp (* fp (fatigue-module-fp-percent module)))
        (setf (fatigue-module-fp module) fp)
      
        (setf finalutility (+ (* alert-utility fp) (if (zerop egs) 0.0 (act-r-noise egs))))       
        finalutility)
      nil)))

(defun fatigue-bla-hook ( chunk )
  "Declarative hook: Applies a fatigue function to the declarative component."
  (let ((module (get-module :fatigue))
        (fd)
        (biomath-prediction))
    (if (and (fatigue-module-enabled module)  (fatigue-module-fd module))
        (let ((alert-bla))
          ;Calculate alert bla
          (setf (fatigue-module-enabled module) nil)
          (with-parameters (:bl-hook nil)
            (setf alert-bla (base-level-activation (get-module declarative) chunk)))
          (setf (fatigue-module-enabled module) t)

          ; Calculate the biomathematical model prediction
          (setf biomath-prediction (compute-biomath-value-for-hour (+ (/ (mp-time) 3600) (fatigue-module-hour module))))
          
          ; Calculate FD from fatigue module.  This is a rough approximation of the values fit 
          ; with the SAST model because SAST was fit using a different biomathematical model
          (setf fd (+ (* biomath-prediction (fatigue-module-fdbmc module)) (fatigue-module-fdc module)))
          
          ; Add in microlapses
          (setf fd (* fd (fatigue-module-fd-percent module)))
          (setf (fatigue-module-fd module) fd)
          
          (* alert-bla fd))
        nil)))

(defun reset-fatigue-module (module)
  "resets the fatigue module with default schedule"
  (setf (fatigue-module-pending module) nil)
  (setf (fatigue-module-last-one-empty module) nil)
  (set-schedule '((6 23)(30 47)(54 84)) :p0 3.841432 :u0 38.509212 :k0 0.019455))

(defun fatigue-module-params (module param)
  "parameter setting and getting for fatigue module"
  (let (mytable)
    (if (consp param)
        (case (car param)
          (:enable-fatigue (setf (fatigue-module-enabled module) (cdr param))
                           ;;; Dan - set the parameters when it's enabled to
                           ;;; make sure they're available in case the modeler or
                           ;;; some other module has manipulated things while it was
                           ;;; turned off, and turn them off when it's disabled to
                           ;;; avoid interfering with other modules that might
                           ;;; want to use them.
                           (cond ((cdr param)
                                  (sgp :utility-hook fatigue-utility-hook :bl-hook fatigue-bla-hook)
                                  
                                  ;; If the utility threshold isn't set then create an event to
                                  ;; set it to ut0 -- do it through an event because the :fp
                                  ;; parameter could be changed between now and when the model
                                  ;; starts running and only want to set :ut if procedural mechanism
                                  ;; is enabled
                                  
                                  (when (null (fatigue-module-ut module))
                                    (schedule-event-now (lambda (m)
                                                          (when (and (fatigue-module-enabled m)
                                                                     (fatigue-module-fp m))
                                                            (sgp-fct (list :ut (fatigue-module-ut0 m)))))
                                                        :maintenance t ;; don't want this to affect the model
                                                        :priority :max ;; happen before the model could try conflict-resolution
                                                        :destination :fatigue ;; pass the current model's fatigue module
                                                        :module :fatigue ;; indicate fatigue caused the event
                                                        :details "Set the initial utility threshold for fatigue"))
                                  
                                  
                                  ;; since we aren't removing the conflict set hook function when fatigue is disabled
                                  ;; only add it if it's not already there
                                  (unless (find 'check-for-fatigue-dec (car (no-output (sgp :conflict-set-hook))))
                                    (sgp :conflict-set-hook check-for-fatigue-dec)))
                                 (t 
                                  (sgp :utility-hook nil :bl-hook nil)
                                  ;; there's no easy way to remove a particular
                                  ;; conflict set hook function so don't worry
                                  ;; about that since with the module disabled 
                                  ;; it doesn't matter.
                                  ))
                           ;; need to return current value
                           (cdr param))
          (:stimulus (setf (fatigue-module-stimulus module) (cdr param))
                     (setf (fatigue-module-fp-percent module) (fatigue-module-stimulus module))
                     (setf (fatigue-module-fd-percent module) (fatigue-module-stimulus module)))
          (:fp-dec (setf (fatigue-module-fp-dec module) (cdr param)))
          (:fp (setf (fatigue-module-fp module) (cdr param)))
          (:fd-dec (setf (fatigue-module-fd-dec module) (cdr param)))
          (:fd (setf (fatigue-module-fd module) (cdr param))) 
          (:ut (setf (fatigue-module-ut module) (cdr param)))
          (:dat (setf (fatigue-module-dat module) (cdr param)))
          
          ;; Dan record this to avoid calling sgp during utility hook for efficiency
          (:egs (setf (fatigue-module-egs module) (cdr param)))
          
          (:fpbmc (setf (fatigue-module-fpbmc module) (cdr param)))
          (:fpmc (setf (fatigue-module-fpmc module) (cdr param)))
          (:fdbmc (setf (fatigue-module-fdbmc module) (cdr param)))
          (:fdc (setf (fatigue-module-fdc module) (cdr param)))
          (:utbmc (setf (fatigue-module-utbmc module) (cdr param)))
          (:utmc (setf (fatigue-module-utmc module) (cdr param)))
          (:ut0 (setf (fatigue-module-ut0 module) (cdr param)))
          (:fp-percent (setf (fatigue-module-fp-percent module) (cdr param)))
          (:fd-percent (setf (fatigue-module-fd-percent module) (cdr param)))
          (:hour (setf (fatigue-module-start-time module) (mp-time))
                 (setf mytable (table (fatigue-module-biomath-class-obj module)))
                 (when (and (numberp (cdr param)) (typep mytable 'hash-table))
                   (if (and (>= (cdr param) (first (ordered-keys mytable)))
                            (<= (cdr param) (car (last (ordered-keys mytable)))))
                       (progn
                         (setf (fatigue-module-hour module) (cdr param)))
                       (progn
                         (print-warning "Hour outside schedule parameters. Fatigue module disabled.")
                         (setf (fatigue-module-enabled module) nil)
                         ))))
          (:biomath-class-obj (setf (fatigue-module-biomath-class-obj module) (make-instance *biomath-class*)))
          (:pending (setf (fatigue-module-pending module) (cdr param)))
          (:last-one-empty (setf (fatigue-module-last-one-empty module) (cdr param)))
          (:bl-hook 
           (when (and (fatigue-module-enabled module)
                      (not (equal (cdr param) 'fatigue-bla-hook)))
             (print-warning "Trying to set BL hook to unknown function for fatigue module. Fatigue module disabled as it will not work properly.")
             (setf (fatigue-module-enabled module) nil)
             )
           )
          (:utility-hook 
           (when (and (fatigue-module-enabled module)
                      (not (equal (cdr param) 'fatigue-utility-hook)))
             (print-warning "Trying to set Utility hook to unknown function for fatigue module. Fatigue module disabled as it will not work properly.")
             (setf (fatigue-module-enabled module) nil)
             )
           )
          (:conflict-set-hook
           (when (and (fatigue-module-enabled module)
                      (not (find 'check-for-fatigue-dec (no-output (car (sgp :conflict-set-hook))))))
             (print-warning "Fatigue module's conflict set hook function was removed. Fatigue module disabled as it will not work properly.")
             (setf (fatigue-module-enabled module) nil)))          
          (:lapse-on-empty
           (setf (fatigue-module-lapse-on-empty module) (cdr param)))
          )
        (case param
          ; Get parameter
          (:enable-fatigue (fatigue-module-enabled module))
          (:stimulus (fatigue-module-stimulus module))
          (:fp-dec (fatigue-module-fp-dec module))
          (:fp (fatigue-module-fp module))
          (:fd-dec (fatigue-module-fd-dec module)) ;; TEH
          (:fd (fatigue-module-fd module))  
          (:ut (fatigue-module-ut module))
          (:dat (fatigue-module-dat module))
          (:fpbmc (fatigue-module-fpbmc module))
          (:fpmc (fatigue-module-fpmc module))
          (:fdbmc (fatigue-module-fdbmc module))
          (:fdc (fatigue-module-fdc module))
          (:utbmc (fatigue-module-utbmc module))
          (:utmc (fatigue-module-utmc module))
          (:ut0 (fatigue-module-ut0 module))
          (:fp-percent (fatigue-module-fp-percent module))
          (:fd-percent (fatigue-module-fd-percent module))
          (:hour (fatigue-module-hour module))
          (:start-time (fatigue-module-start-time module))
          (:biomath-class-obj (fatigue-module-biomath-class-obj module))
          (:lapse-on-empty (fatigue-module-lapse-on-empty module))
          ))))


(define-module-fct :fatigue 
                   nil
  (list (define-parameter :enable-fatigue :valid-test 'tornil :default-value 'nil :warning "T or NIL"  :documentation "Enable the fatigue mechanism")
        (define-parameter :stimulus :valid-test 'numberp :default-value 1.0d0 :warning "a number" :documentation "Most recent stimulus (0-1)")
        (define-parameter :fp-dec :valid-test 'numberp :default-value 0.98d0 :warning "a number" :documentation "Procedural arousal decrement")
        (define-parameter :fp :valid-test 'numornil :default-value 1.0d0 :warning "a number or nil to disable procedural mechanism" :documentation "Current procedural arousal value")
        (define-parameter :fd-dec :valid-test 'numberp :default-value 0.0d0 :warning "a number" :documentation "Declarative arousal decrement")
        (define-parameter :fd :valid-test 'numornil :default-value nil :warning "a number or nil to disable declarative mechansim" :documentation "Current delcarative arousal value") 
        (define-parameter :ut :owner nil)
        (define-parameter :dat :owner nil)
        (define-parameter :bl-hook :owner nil)
        (define-parameter :utility-hook :owner nil)
        (define-parameter :conflict-set-hook :owner nil)
        (define-parameter :egs :owner nil)
        (define-parameter :fpbmc :valid-test 'numberp :default-value 0 :warning "a number" :documentation "Coefficient relating fp to biomathematical model")
        (define-parameter :fpmc :valid-test 'numberp :default-value 0 :warning "a number" :documentation "Coefficient relating fp to minutes on task")
        (define-parameter :fdbmc :valid-test 'numberp :default-value -0.02681d0 :warning "a number" :documentation "Coefficient relating fd to biomathematical model")
        (define-parameter :fdc :valid-test 'numberp :default-value 0.95743d0 :warning "a number" :documentation "Coefficient relating fd to a constant offset")                                                      
        (define-parameter :utbmc :valid-test 'numberp :default-value 0 :warning "a number" :documentation "Coefficient relating ut to biomathematical model")
        (define-parameter :utmc :valid-test 'numberp :default-value 0 :warning "a number" :documentation "Coefficient relating ut to minutes on task")
        (define-parameter :ut0 :valid-test 'numberp :default-value 0 :warning "a number" :documentation "Initial utility threshold value")
        (define-parameter :fp-percent :valid-test 'numberp :default-value 1 :warning "a number" :documentation "FP-Percent")  ;BZV
        (define-parameter :fd-percent :valid-test 'numberp :default-value 1 :warning "a number" :documentation "FD-Percent")   ;BZV
        (define-parameter :hour :valid-test 'numberp :default-value 0 :warning "a number" :documentation "The number of hours since midnight of first day of experiment")
        (define-parameter :start-time :valid-test 'numberp :default-value 0 :warning "a number" :documentation "The start time of the task")
        (define-parameter :biomath-class-obj :valid-test 'numberp :default-value 0 :warning "a number" :documentation "A biomathematical object to hold alertness values")
        (define-parameter :pending :valid-test 'tornil :default-value 'nil :warning "T or NIL"  :documentation "Check that microlapse is pending")
        (define-parameter :lapse-on-empty :valid-test 'tornil :default-value nil :warning "T or NIL" :documentation "Whether or not a microlapse will happen when the conflict-set is empty.")
        )
  
  :creation (lambda (x) (declare (ignore x)) (make-fatigue-module))
  :reset (list nil 'reset-fatigue-module)
  :params 'fatigue-module-params
  :version "3.3"
  :documentation "Fatigue mechanism using McCauley 2013 biomath model and declarative fatigue implemented")


;;; Dan: Added to support the require-extra command since it's
;;; based on the standard require functionality.

(provide "fatigue")

#|
This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
|#
