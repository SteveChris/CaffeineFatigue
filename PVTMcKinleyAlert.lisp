;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Model and Batch Run Code for Psychomotor Vigilance Task (PVT)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Glenn Gunzelmann, Bella Z. Veksler
;;; Copyright   : (c)2016 AFRL/HEAS, All Rights Reserved
;;; Availability: tbd
;;; Address     : AFRL/RHAC
;;;             : 711 Human Performance Wing
;;;             : 2620 Q Street (Bldg 852)
;;;             : Wright-Patterson AFB, OH 45431-7905
;;;             : bellav717@gmail.com
;;;             : glenn.gunzelmann@us.af.mil
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; History
;;;
;;;  2004-07-21 - Glenn Gunzelmann
;;;             : First version.
;;;             : Runs PVT model, apparently across variations in any act-r
;;;               or rpm parameters where real-numbered values are used
;;;
;;;  2004-07-31 - Kevin Gluck
;;;             : There is a bug preventing variation in the :dat parameter from having the intended effect
;;;             : The randomize-time parameter is having the opposite effect of that predicted by the documentation
;;;
;;;             : Dan Bothell provided fix for the randomize-time parameter
;;;             : Mike Byrne subsequently provided a new master-process file and a new rpm-parameters file
;;;
;;;             : Dan recommends setting up a list of parameters and having ACT-R read off that list
;;;             : Important thing is to set :dat after reset and before defining the productions
;;;               -(i.e., in an sgp after clear-all)
;;;
;;;  2004-08-25 - Glenn Gunzelmann
;;;             : Added additional keywords to pvt-batch function to make the
;;;               the system more flexible. The following keywords were added:
;;;               -:lapse-threshold - rts greater than this are considered lapses
;;;               -:session-length - number of seconds to run each pvt session
;;;               -:interval - reporting interval for rts in batch run file
;;;               -:ind-data - flag; if t, individual data files are created for
;;;                each run of the model, with complete rt & delay legacies
;;;
;;;             : Changed code for running model once stimulus appears. Model now runs in
;;;               distinct chunks of time, controled by the :dat parameter. This was needed
;;;               to force ACT-R to recalculate expected utilities, even after finding no
;;;               utilities >=0 for a given cycle
;;;
;;;             : Removed "waiting" production. Delays now occur when no task-related productions
;;;               rise above threshold, resulting in "blank" cycles.
;;;               -Depends on previous revision.
;;;
;;;  2004-12-07 - Glenn Gunzelmann
;;;             : Added functionality to pvt-batch to support comparison to data
;;;               in batch runs
;;;               -A third file is created to record statistics and average data
;;;                when multiple iterations are run of a single parameter set
;;;               -Correlations and mean-deviations are printed out for each
;;;                set of parameters as the model goes through a batch run
;;;                -These statistics are calculated for each day of the experiment
;;; 
;;;             : "Wait" production is back, based on experimenting with the model
;;;
;;;             : Added probabilities of success for the productions
;;;
;;;             : Added a keyword in pvt-batch to manipulate a parameter which
;;;               increments :c for the respond productions each time "Wait" fires
;;;               -Briefly used to manipulate G, and it may be reverted to that
;;;                purpose
;;; 
;;;  2004-06-11 - Glenn Gunzelmann
;;;             : Added function to support multiple small batch runs
;;;               -Allows for variation of UT around G, rather than running the
;;;                entire parameter space, where many combinations are nonsensical
;;;
;;; 2005-07-14 JBG Old code version w/out queuing (for performance reasons)
;;;                but w/ correct data
;;;
;;; 2005-08-18 - Glenn Gunzelmann
;;;            : Added total responses, median RT, Standard Deviation to batch-data
;;;              and statistics files written during batch runs
;;;            : Rewrote statistic generation functions for increased efficiency
;;;            : Eliminated comparison to data in batch run (r and RMSD for data
;;;              averaged over entire days
;;;            : Verified accuracy of data recording relative to human data summary
;;;
;;; 2007-10-25 - Rick More
;;;            : Added new utility mechanisms fp and fp-dec
;;;
;;; 2008-4-29 - Rick More
;;;            : Adjusted fatigue mechansim so that noise is not scaled
;;;            : Reset *fp* across trials
;;;
;;; 2009-02-12 - Rick More
;;;            : Changed fatigue mechanisms to Dan Bothel's module
;;;
;;; 2009-05-05 - Rick More
;;;            : Changed run-full-time to run during delay period.
;;;
;;; 2009-05-05 - Rick Moore
;;;            : Added run-model function
;;;
;;; 2012-09-28 - Rick Moore
;;;            : Modified to work with new run-with-microlapses function
;;;
;;; 2013-04-18 - Bella Veksler
;;;            : Combined model and task code into one file
;;;            : New running instructions
;;;            : Modified run-model function and added new write out function
;;;            : to work with MindModeling
;;;
;;; 2016-10-30 - Bella Veksler
;;;            : Added debugging code and comments for distribution, organized file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Running Instructions
;;;
;;;  1. Call (run-model numiterations modelduration minper)
;;;            where numiterations is number of times to rerun the model
;;;            modelduration indicates how long the model should run in minutes
;;;            minper indicates how finely to divide the data into blocks (ie 1 min blocks)
;;;    Example: (run-model 10 10 1) will run the model 10 times, for 10 min each 
;;;            and report the mean RT, meadian RT, # lapses, # trials, # false starts
;;;            and # sleep attacks for each minute 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   From ACT-R/PM website ...
;;;
;;;   :randomize-time nil or number
;;;   Default NIL. Normally, completion times for perceptual-motor operations have fixed times. 
;;;   To make those times vary randomly around those fixed values, set this to a positive integer. 
;;;   A value of 2 will produce an output from 1/2 to 3/2 of the input for each time; 
;;;   a value of 4 will produce an output from 3/4 to 5/4, and so forth. 
;;;   If no number is supplied (e.g., this is just set to T, then the value 3 is used, which is the EPIC default. 
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Target parameters for simulating effects of sleep restriction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   cognitive cycle time :dat  **
;;;   activation noise     :ans
;;;   expected gain noise  :egs  *
;;;   goal value           :g    **
;;;   goal activation (W)  :ga
;;;   latency factor       :lf
;;;   motor system params?
;;;   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Global parameters for running experiment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Window features
(defparameter *w* nil)

;;Other Experiment Parameters
(defparameter *response* nil)
(defparameter *actr-enabled-p* t)

;;for local run to help debug
(defparameter *local* t)
(defparameter *traceon* nil)
(defparameter *trialresponses* (translate-logical-pathname "ACT-R:PVT-outputs;trialresponses.txt"))   ;outputs each trial's RT and parameter values
(defparameter *blocksummary* (translate-logical-pathname "ACT-R:PVT-outputs;blocksummary.txt"))  ;outputs summary of RT, lapses, false starts, sleep attacks per block indicated in call to run-model
(defparameter *trace* (translate-logical-pathname "ACT-R:PVT-outputs;trace.txt"))   ;outputs all task events and production firings

;;parameters for study
(defparameter *dat* 0.04) 
(defparameter *iu* 2.6)
(defparameter *ut0* 2.2)
(defparameter *fpbmc* .02)          
(defparameter *fpmc* 0)             
(defparameter *utbmc* .01)          
(defparameter *utmc* 0)             
(defparameter *hour* 8)
(defparameter *fp-dec* 0.98)  
(defparameter *egs* 0.25)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helper Functions
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;percent of distribution in list l that falls below value p
(defun pctless(l p)
    (float (/ (count p l :test #'>) (length l))))

(defun square(n) (* n n))

;rmse except ignoring quantile boundaries that are too far
(defun rmse (l1 l2)
  (if (eql (length l1) (length l2))
	(let ((l12 (subseq l1 1 14))
		(l22 (subseq l2 1 14))
		)
      (sqrt (/ (ssq l12 l22) (length l12)))
	  )
      -1
      ))

;root mean squared error between the two lists
;(defun rmse (l1 l2)
;  (if (eql (length l1) (length l2))
;      (sqrt (/ (ssq l1 l2) (length l1)))
;      -1
;      ))

;numerator when calculating R^2
;sum of squared errors (explained variance)
(defun ssq (l1 l2)
  ;(print "doing ssq")
  (if (eql (length l1) (length l2))
	
	(sum (mapcar (lambda(i j) (square (- i j))) l1 l2))
       
	-1
	)
)

(defun sum (values)
	(let ((sum 0))
		(mapcar #'(lambda (value)
					(setf sum (+ value sum)))
		  values)
	 sum
)
)

;total variance between two lists
;list 1 should be the actual and 2 the predicted
(defun totalVariance (act pred)
	;(print "denominator when calcuating R^2")
	(let (
		(m (mean act))
	  )
      (sum (mapcar (lambda(i) (square (- i m))) pred))
 )
)

(defun rSquared (act pred)
	(- 1 (/ (ssq act pred) (totalVariance act pred) ))
	
)

(defun get-production-names ()
 (mapcar (lambda(x) (production-name x)) (procedural-productions (get-module procedural))))

(defun get-utilities()
  (let ((prods (get-production-names))
        (ret nil))
    (dolist (p prods)
      (push (gethash 'utility (production-parameter-values (get-production p))) ret))
    (reverse ret)))

(defun get-u()
  (let ((prods (get-production-names))
        (ret nil))
    (dolist (p prods)
      (push (gethash 'u (production-parameter-values (get-production p))) ret))
    (reverse ret)))

(defun flatten (l)
  (cond ((null l) nil)
        ((atom l) (list l))
        (t (loop for a in l appending (flatten a)))))

(defun write-list-to-file (l filename)
  "Write a list to a file"
  (with-open-file (fileout filename :direction :output :if-exists :append :if-does-not-exist :create)
    (setf l (flatten l))
    (loop for i to (- (length l) 2) do          
          (format fileout "~10f, " (nth i l))
	 )
    (format fileout "~10f ~%" (car (last l))) ;SCJ unsure why, but this line used to add a tab to the end. (format fileout "~10f ~c~%" (car (last l)) #\tab)
  )
)

;returns median of values
(defun median (values)
  (when values
    (let* ((srtd (sort values '<))
           (middle (/ (length srtd) 2)))
      (if (oddp (length srtd))
          (nth (floor middle) srtd)
        (/ (+ (nth middle srtd) (max (nth (1- middle) srtd) 0)) 2.0))
      )))

;returns standard deviation of values
(defun stdev (values)
  (let ((avgrt (mean values)) (summation 0))
    (mapcar #'(lambda (value)
                (setf summation
                  (+ summation
                     (* (- value avgrt) (- value avgrt)))))
      values)
    (if (> (length values) 1) (sqrt (/ summation (1- (length values)))) 0)
    ))

;returns mean of values
(defun mean (values)
    (if (> (length values) 0)
		(/ (sum values) (length values) 1.0)
	0)
)
	
;returns a fraction that's a better round to a given decimal place
(defun round-to (number precision &optional (what #'round))
    (let ((div (expt 10 precision)))
         (/ (funcall what (* number div)) div)))
		 

(defun current-interval (start)
  (let ((cur-time (if *actr-enabled-p* (get-time) (get-internal-real-time))))
    (/ (- cur-time start) 1000)
    ))

(defun print-screen ()
  "Print the Vision Module's visicon sorted by left to right top to bottom. For debugging."
  (awhen (get-module :vision)  ;; Test that there is a vision module
         (update-new it)
         (check-finsts it) 
      ;   (command-output "Loc        Att   Kind           Value             Color           ID")
       ;  (command-output "---------  ---   -------------  ----------------  --------------  -------------")
         (let ((chunks (copy-list (visicon-chunks it))))
           (setq chunks (stable-sort chunks #'< :key (lambda (x) (chunk-slot-value-fct x 'screen-y))))
       ;    (mapcar 'print-icon-feature chunks)
         (mapcar #'(lambda(chunk) (list chunk (feat-attended chunk (get-module :vision)))) chunks))))

;useful for debugging, writes out trace of task and model execution
(defun write-trace (label thisfile start-time)
  (if (and *traceon* *local*) 
      (if *response* 
          (write-list-to-file (list (mp-time) label *response* (round (- *response* start-time)) (no-output (sgp :fp :fp-percent :ut :iu)) (get-utilities) (print-screen) (buffer-read 'visual-location) (mywhynot '(attend)) (no-output (sgp :fp-dec))) thisfile)
          (write-list-to-file (list (mp-time) label 0 0 (no-output (sgp :fp :fp-percent :ut :iu)) (get-utilities) (print-screen) (buffer-read 'visual-location) (mywhynot '(attend)) (no-output (sgp :fp-dec))) thisfile))
      ))

(defun mywhynot (productions)
  (if (current-model) 
      (let* ((procedural (get-module procedural))
             (conflict-set (no-output (pmatches-internal procedural)))
             (ret '()))
        
        (dolist (production-name (if (null productions)
                                     (all-productions)
                                   productions))
          
          (let ((production (get-production production-name)))
            (if (null production)
                (push (format nil "~s does not name a production." production-name) ret)
              (if (production-disabled production)
                  (push (format nil "Production ~s is disabled." production-name) ret)
                (if (member production-name conflict-set)
                    (if (and (procedural-ppm procedural) (production-partial-matched-slots production))
                        (progn ;; It's only a partial match
                          (push (format nil "Production ~s partially matches the current state:" production-name) ret)
                         ; (print-instantiation production)
                          (let ((ut (car (no-output (sgp :ut)))))
                            (when (and (numberp ut)
                                       (numberp (production-utility production-name))
                                       (< (production-utility production-name) ut))
                              (push (format nil "Utility was below the threshold the last time it was in the conflict set.") ret ))))
                      (progn ;; It's a complete match
                        (push (format nil "Production ~s matches:" production-name) ret)
                        (let ((ut (car (no-output (sgp :ut)))))
                          (when (and (numberp ut)
                                     (numberp (production-utility production-name))
                                     (< (production-utility production-name) ut))
                            (push (format nil "Utility was below the threshold the last time it was in the conflict set.") ret)))))
                  (progn
                    (push (format nil "Production ~s does NOT match." production-name) ret)
                    (push (format nil "It fails because: ") ret)
                    (push (format nil (failure-reason-string (production-failure-condition production) production)) ret)   ;actr7
                    ;(push (format nil (failure-reason-string (production-failure-condition production) procedural production)) ret)   ;actr6
                    )
                  )))))
        ret)
    (print-warning "Whynot called with no current model.")))
	
	;SCJ Load PBPK caffeine concentration values
(defun get-file (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
          while line
          collect line)))
		  
		  


;TODO I wish these could be before the helped functions, but we need use helper functions to read+format data
(defparameter caff_concs (get-file "PBPK outputs/caffModelOuts.txt"))
(defparameter Tss nil) ;Tss are timestamps in hours starting from when caffeine begins to be administered
(defparameter Concs nil)
(let (comma)
  (loop for entry in (nreverse caff_concs) ;Todo might actually be meaningless to reverse this (because I can index forward or backward for improved speed algorithm)
  do
    (setq comma (position #\, entry)) ;always the same position, but I'm going to get position anyways, for sake of robustness
	(push (subseq entry 0 comma) Tss)
 	(push (subseq entry (+ 1 comma) (- (length entry) 1)) Concs)
  )
)
(print "loaded caffeine list")
;;quantile boundaries from human data against McKinley (note that these values are arbitrary simply a nonparametric way to capture the shape of an RT distribution)
; A good-ish way to generate a similar set of bins is to generate evenly-spaced bins between 150 (min we care about) and reasonable upper limit
;TODO: check what happens to things outside 845
(defparameter ALLPVT (get-file #P"Empirical/ALLPVT.csv"))
(defparameter humanbins '(150 235.1570 242.8643 250.0535 255.0944 261.1778 265.6104 270.8033 274.8513 280.4543 285.0058 288.4825 294.4158 300.5963 306.8612 315.3008 325.4594 335.2695 351.6218 382.5690 845.4621))

;;cumulative distributions of all empirical responses falling within each of the humanbins quantiles
(defparameter humanquantiles_both (make-hash-table))
(defparameter humanquantiles_decaf (make-hash-table))
(defparameter humanquantiles_caff (make-hash-table))
;need to set rts to whatever I pull in from the csv.
;maybe I want to cache it and maybe I want to read it in every time. Depends how long it takes


;but for now, all I need is a klugey way to get rt1
(let (
	;(Time)		;column headers, should I want them
	(RT)
	(humanraw_caff (make-hash-table))
	(humanraw_decaf (make-hash-table))
	(humanraw_both (make-hash-table))
	;(Subject)
	(bout)
	(b)
	(hour)
	;(bouttime)
	(condition)
	(lcomma)
	(ncomma)
	)
(loop for entry in ALLPVT ;Time,RT,Subject,Bout,Date,bouttime,condition,hour
  do
    (setq lcomma (position #\, entry))
	;(push (subseq entry 0 lcomma) Time)
	(setq lcomma (+ 1 lcomma))
	(setq ncomma (position #\, entry :start lcomma))
	(push (read-from-string (subseq entry lcomma ncomma)) RT)
	(setq lcomma (+ 1 ncomma))
	(setq ncomma (position #\, entry :start lcomma)) ;subject
	;(push (subseq entry lcomma ncomma) Subject) ;etc to get the rest of the parameters if I want them. May need to finagle + or - 1 and/or typing
	(setq lcomma (+ 1 ncomma))
	(setq ncomma (position #\, entry :start lcomma)) 
	(push (read-from-string (subseq entry lcomma ncomma)) bout)
	(setq lcomma (+ 1 ncomma))
	(setq ncomma (position #\, entry :start lcomma))
	(push (read-from-string (subseq entry lcomma ncomma)) hour)
	(setq lcomma (+ 1 ncomma))
	(setq ncomma (position #\, entry :start lcomma)) ;bouttime
	(setq lcomma (+ 1 ncomma))
	(push (subseq entry lcomma (- (length entry) 1 )) condition) ;-1 so I'm not picking up line feed or carriage return or whatever
)

(dotimes (i (length bout))
	(setf b (elt Bout i))
	;Push all the reaction time data into a hash table
	(push (elt RT i) (gethash b humanraw_both))
	(if (equal (elt condition i) "caffeine")
		(push (elt RT i) (gethash b humanraw_caff))
		(push (elt RT i) (gethash b humanraw_decaf)) ;condition is either caff or decaf so we can if-else (as far as I know)
	)
)
(format t "loaded all ~D lines of PVT responses" (length RT))
;guilty hardcode because I know that there are 9 bouts
(dotimes (i 9) ;put the raw data into quantile form so we can compare distributions
	(setf (gethash i humanquantiles_caff) (mapcar #'(lambda (j) (pctless (gethash (+ 1 i) humanraw_caff) j)) humanbins))
	(setf (gethash i humanquantiles_decaf) (mapcar #'(lambda (j) (pctless (gethash (+ 1 i) humanraw_decaf) j)) humanbins))
	(setf (gethash i humanquantiles_both) (mapcar #'(lambda (j) (pctless (gethash (+ 1 i) humanraw_both) j)) humanbins))
)
	(terpri)
(format t "hashed all ~D lines of PVT responses" (length RT))
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Experiment Delivery Code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pvt (subject hour duration &key (visible nil) (sleep-threshold 30000))
  (sgp :enable-fatigue t) ;SCJ added this line. Not sure if it really helps
  (let ((data nil)
        (trial 0)
        (start-block (if *actr-enabled-p*
                         (get-time)
                         (get-internal-real-time)))
        (delay 0)
        (start-time nil)
	(fpdec (car (no-output (sgp :fp-dec )))))
    (setf *w* (open-exp-window "Task Window" :x 200 :y 200 :width 200 :height 200 :visible visible))
    (install-device *w*)
    (if *actr-enabled-p*
        (progn
          ;(sgp :enable-fatigue t) ;SCJ made this active. not sure why it was inactive before
          (run-full-time 1))
        (sleep 1))
    (while (< (current-interval start-block) duration)
           (clear-exp-window)
           ;(if *actr-enabled-p* (sgp :enable-fatigue nil)) 
           (proc-display)
           (no-output (eval (list 'mod-chunk (goal-focus) 'state 'wait)))
           (reset-stimulus)
           (setf delay (+ 2 (act-r-random 9)))
           (setf *response* nil)
           (setf start-time (if *actr-enabled-p* (get-time) (get-internal-real-time)))
           
           ;;DELAY
           (eval (list 'sgp ':fp-dec 1))   ; set to 0 in subtractive, to 1 in multiplicative
           ;(if *actr-enabled-p* (sgp :enable-fatigue t))
           (clear-buffer 'visual-location)
           (clear-buffer 'visual)
           (clear (get-module :vision))
           
           (write-trace 'delaybegins *trace* start-time)
           (run-until-time-or-condition delay #'(lambda() (not (equal nil *response*))))
           (eval (list 'sgp ':fp-dec fpdec))
           (write-trace 'fpdecon *trace* start-time)
           
           ;;Handle False Starts
           (when *response* 
             (write-trace 'falsestart *trace* start-time)	        
             (if *local* (write-list-to-file (list subject (floor (/ hour 24)) (mod hour 24) (mp-time) (no-output (sgp :fpbmc :fpmc :utbmc :utmc :dat :ut0 :iu :fp :fp-percent :ut :ppm)) 0 (no-output (sgp :fp-dec))) *trialresponses* ))
             (setf data (cons (list delay 0 (get-time)) data))
             (setf *response* nil)
             )
           
           ;continue trial and give model chance to respond
           ;;STIMULUS PRESENTATION
           (when (null *response*)
             (setf start-time (if *actr-enabled-p* (get-time) (get-internal-real-time)))
             (add-text-to-exp-window :text (format nil "START") :color 'red :x 100 :y 100 :width 80 :height 80)
             (proc-display)
             (write-trace 'stimulus *trace* start-time)
             (no-output (eval (list 'mod-chunk (goal-focus) 'state 'wait)))
             (write-trace 'trytorespond *trace* start-time)
             (run-until-time-or-condition (/ sleep-threshold 1000) #'(lambda() (not (equal nil *response*))))
             (write-trace 'timeexpire *trace* start-time)
             (clear-exp-window)
             ;(if *actr-enabled-p* (sgp :enable-fatigue nil))
             (proc-display))
           
           ;;Handle Sleep Attacks
           (if (and (null *response*) *actr-enabled-p*) 
               (progn 
                 (write-trace 'sleepattack *trace* start-time)
                 (setf *response* (get-time)))
               (write-trace 'alert *trace* start-time))
           
           
           ;;Record RT
           (setf data (cons (list delay (round (- *response* start-time)) (get-time)) data))
           (if *local* (write-list-to-file (list subject (floor (/ hour 24)) (mod hour 24) (mp-time) (no-output (sgp :fpbmc :fpmc :utbmc :utmc :dat :ut0 :iu :fp :fp-percent :ut :ppm)) (round (- *response* start-time)) (no-output (sgp :fp-dec))) *trialresponses*))			
           
           ;;Feedback
           (add-text-to-exp-window :text (format nil "~a" (round (- *response* start-time))) :color 'blue :x 80 :y 90 :width 40 :height 20)
           
           (if *actr-enabled-p*
               (run-full-time .5)
               (sleep .5))
           (setf trial (1+ trial)))
    (clear-exp-window)
    (add-text-to-exp-window :text "Done" :color 'red :x 80 :y 90 :width 40 :height 20)
    (reverse data)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Running Model Code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;run-model runs the model multiple times and reports the results
;specify: numiterations: number of times to run model
;         modelduration: duration of model in minutes 
;         minper: how many minutes per block to write out the summary
;         dat: cycle time (0.04)
;         iu: initial utility (2.6)
;         ut0: utility threshold (2.2)
;         fpdec: decrement multiplier due to microlapses (0.98)
;         fpmc: Production Utility Time on Task exponent parameter (default=0)
;         utmc: Utility Threshold Time on Task exponent parameter (default=0)

;to run the model:
;SCJ There's a new way in town to run the model
;(iterateEGS .2 .05 .3)
(defun run-model (targquantiles &key (dat 0.04) (iu 2.6) (ut0 2.2) (fpdec 0.98) (fpmc 0) (fpbmc 0) (utmc 0) (utbmc .1) (numiterations 1) (modelduration 10) (minper 10) (egs 0.25))
  (setf *egs* egs)
  (setf *dat* dat) 
  (setf *iu* iu)
  (setf *fpbmc* fpbmc)  
  (setf *fpmc* fpmc)
  (setf *utbmc* utbmc)
  (setf *utmc* utmc)
  (setf *ut0* ut0)
  (setf *fp-dec* fpdec)
  (let ((run-data '())
        (bouts '(65.85512973	67.84515309	69.75257488	71.81277052	73.76905808	75.85810963	77.84786624	79.90473574	81.88345915)) ;hardcoded, possible to softcode if I read in and average out all the hours from ALLPVT
   ) ;bouts are based on hour number from beginning of schedule (10 = 10am on first day)
    (if *local* 
        (progn 
          (write-list-to-file (list "subject" "day" "hour" "min" "fpbmc" "fpmc" "utbmc" "utmc" "dat" "ut0" "iu" "fp" "fp-percent" "ut" "ppm" "RT" "fp dec") *trialresponses*)  ;BMH=*last-biomathematical-hour* BMV=*last-biomathematical-value*
          (write-list-to-file (list "subjects" "day" "hour" "min" "fpbmc" "fpmc" "utbmc" "utmc" "dat" "ut0" "iu" "egs" "meanalertRT" "medianalertRT" "lapsespct" "fapct" "sapct" "numtrials" "rmse") *blocksummary*) 
          (write-list-to-file (list 'time 'label 'response 'rt 'fp 'fp-percent 'ut 'iu 'wait 'attend 'respond) *trace*)))
    (dotimes (b (length bouts))
      (setf run-data '()) ;run-data reset after every bout because it is analyzed by block
      (dotimes (i numiterations) 
        (format t "Subject: ~s Bout: ~s (hour ~s)" i b (elt bouts b)) 
        (terpri)
        (reset)
        ;hardcode set schedule. It doesn't even do anything here. The lifting is done by fatigue.lisp
		(set-schedule '((6 23)(30 47)(54 84)) :p0 3.841432 :u0 38.509212 :k0 0.019455) ;hours of schedule set according to McKinley methods and SCJ added 2 to the end so that I can see biobehave
		(setf *hour* (+ (elt bouts b) 2)) ;SCJ temporarily set so that I can see if I get biomath behavior better
		(sgp-fct (list :hour *hour*))
		
		;(setf *hour* (+ 70 b)) ; for shenaniganry testing
		;(print (compute-biomath-value-for-hour *hour*)) ;test that biomath value is actually doing something
		
		(let ((cur-run-data (pvt i *hour* (- (* 60 modelduration) 10) :sleep-threshold 30000 :visible nil)))
          (setf run-data (append cur-run-data run-data)))
	   
      )
	
	(mm-write-out_bout run-data modelduration minper numiterations targquantiles b) 
    )
  )
 )
 
;Very similar to basic run-model
;but targquantiles defaults to caffeinated
;and only the bouts after 5 run, since thats when the empirical data become significantly different
;and rmse writes cleaner into blocksummary
(defun run-modelEGS (targquantiles &key (dat 0.04) (iu 2.6) (ut0 2.2) (fpdec 0.98) (fpmc 0) (fpbmc 0) (utmc 0) (utbmc .1) (numiterations 1) (modelduration 10) (minper 10) (egs 0.25))
  (setf *egs* egs)
  (setf *dat* dat) 
  (setf *iu* iu)
  (setf *fpbmc* fpbmc)  
  (setf *fpmc* fpmc)
  (setf *utbmc* utbmc)
  (setf *utmc* utmc)
  (setf *ut0* ut0)
  (setf *fp-dec* fpdec)
  (let ((run-data '())
        (bouts '(65.85512973	67.84515309	69.75257488	71.81277052	73.76905808	75.85810963	77.84786624	79.90473574	81.88345915)) ;hardcoded, possible to softcode if I read in and average out all the hours from ALLPVT
   ) ;bouts are based on hour number from beginning of schedule (10 = 10am on first day)
    (if *local* 
        (progn 
          (write-list-to-file (list "subject" "day" "hour" "min" "fpbmc" "fpmc" "utbmc" "utmc" "dat" "ut0" "iu" "fp" "fp-percent" "ut" "ppm" "RT" "fp dec") *trialresponses*)  ;BMH=*last-biomathematical-hour* BMV=*last-biomathematical-value*
          (write-list-to-file (list "subjects" "day" "hour" "min" "fpbmc" "fpmc" "utbmc" "utmc" "dat" "ut0" "iu" "egs" "meanalertRT" "medianalertRT" "lapsespct" "fapct" "sapct" "numtrials" "rmse") *blocksummary*)
          (write-list-to-file (list 'time 'label 'response 'rt 'fp 'fp-percent 'ut 'iu 'wait 'attend 'respond) *trace*)))
    (dotimes (b (length bouts))
     (if (> b 4) (progn ;SCJ added this so we can control which bouts we look at and not have to run EVERYTHING because we only care about bout 6 and onwards
      (setf run-data '()) ;run-data reset after every bout because it is analyzed by block
      (dotimes (i numiterations) 
        (format t "Subject: ~s Bout: ~s (hour ~s)" i b (elt bouts b)) 
        (terpri)
        (reset)
        ;hardcode set schedule. It doesn't even do anything here. The lifting is done by fatigue.lisp
		(set-schedule '((6 23)(30 47)(54 84)) :p0 3.841432 :u0 38.509212 :k0 0.019455) ;hours of schedule set according to McKinley methods and SCJ added 2 to the end so that I can see biobehave
		(setf *hour* (+ (elt bouts b) 2)) ;SCJ temporarily set so that I can see if I get biomath behavior better
		(sgp-fct (list :hour *hour*))
		
		;(setf *hour* (+ 70 b)) ; for shenaniganry testing
		;(print (compute-biomath-value-for-hour *hour*)) ;test that biomath value is actually doing something
		
		(let ((cur-run-data (pvt i *hour* (- (* 60 modelduration) 10) :sleep-threshold 30000 :visible nil)))
          (setf run-data (append cur-run-data run-data)))
	   
      )
	;(print "about ot write out")
	(mm-write-out_bout run-data modelduration minper numiterations targquantiles b) 
	;(print "wrote out")
    )))
  )
 )

;run the model, iterating over utbmc levels so that we can find which are best
;TIME AWAKE FATIGUE WAS OFFFFFFFFFFFFFF BEFORE
;values criss-crossed from Walsh 2017 paper. This works when TOT parameters are OFF
;U = initial utility = iu
;T = initial utility threshold = ut0
;ap = utility scaling slope = fpbmc
;at = Threshold scaling slope = utbmc
;FPdec = utility decrement after lapse = fpdec
(defun iterateUTBMC (start stepSize end &key (tq humanquantiles_caff) (dat 0.04) (iu 5.07) (ut0 4.59) (fpdec 0.984) (fpmc 0) (fpbmc -.018) (utmc 0) (egs 0.25) (numiterations 10) (modelduration 10) (minper 10))
	
 (let ((nSteps (round (/ (- end start) stepSize)))
	   (currUTBMC start)
	   (baseTR *trialresponses*) ;making it so I can read from a whole bunch of files after iteration
      )
	  
 (dotimes (currStep nSteps) 	;because this is a numerical method, there is some slippage, but I don't think it's enough to matter
	
	(terpri)
	(format t ":utbmc value: ~s" currUTBMC) 
	(terpri)
	;make and run models with different filenames (hopefully parallelizeable, since there's no overlap / reliance between running models)
	(setf *trialresponses* (concatenate 'string (namestring baseTR) "UTBMC" (write-to-string currUTBMC))) 
	(run-model tq :dat dat :iu iu :ut0 ut0 :fpmc fpmc :fpbmc fpbmc :utmc utmc :utbmc currUTBMC :numiterations numiterations :modelduration modelduration :minper minper :fpdec fpdec :egs egs)
	(setf currUTBMC (float (round-to (+ currUTBMC stepSize) 4))) ; the slippage is why I'm rounding here - I'm rounding to 4th decimal place
 )
 (setf *trialresponses* baseTR) ; put things back the way they were, in case I run other commands after or something
 )
)


;run the model, iterating over egs levels so that we can find which are best
;very similar to 
(defun iterateEGS (start stepSize end &key (app nil) (tq humanquantiles_caff) (dat 0.04) (iu 5.07) (ut0 4.59) (fpdec 0.984) (fpmc 0) (fpbmc -.018) (utmc 0) (utbmc -0.022) (numiterations 10) (modelduration 10) (minper 10))
 (setf *blocksummary* (concatenate 'string (namestring *blocksummary*) app))
 (let ((nSteps (round (/ (- end start) stepSize)))
	   (currEGS start)
	   (baseTR *trialresponses*) ;making it so I can read from a whole bunch of files after iteration
      )
	  
 (dotimes (currStep nSteps) 	;because this is a numerical method, there is some slippage, but I don't think it's enough to matter
	
	(terpri)
	(format t ":egs value: ~s" currEGS) 
	(terpri)
	;make and run models with different filenames (hopefully parallelizeable, since there's no overlap / reliance between running models)
	(setf *trialresponses* (concatenate 'string (namestring baseTR) "EGS" (write-to-string currEGS))) 
	(run-modelEGS tq :dat dat :iu iu :ut0 ut0 :fpmc fpmc :fpbmc fpbmc :utmc utmc :utbmc utbmc :numiterations numiterations :modelduration modelduration :minper minper :fpdec fpdec :egs currEGS)
	(setf currEGS (float (round-to (+ currEGS stepSize) 4))) ; the slippage is why I'm rounding here - I'm rounding to 4th decimal place
 )
 (setf *trialresponses* baseTR) ; put things back the way they were, in case I run other commands after or something
 )
)


;write out results in MindModeling format, includes mean, median, lapses, 
;false starts and sleep attacks per period
;Same as above, but indexes the hash table based on bout number instead of model duration
;TODO make hashtable access a combination of bout number AND model duration so that we can account for time on task AND time-awake fatigue
(defun mm-write-out_bout(x modelduration minper subjects targquantiles boutNum)
  (let* (;(milper (* 1000 60 minper))  ;milliseconds in a period
         (my-hash-responses (make-hash-table))
         (DVs (make-hash-table :test 'equal))
         myhist
         fs
         alert
         lapse
		 rms
         sa
         rts
		 )
    (dolist (i x) ;SCJ for each response produced by the model, store it in that hash table
		;h is normally the block index ; it is calculated using the time of the PVT trial
		;TODO right now it's kluged to just be the bout number
      (let ((h boutNum))
          (if (gethash h my-hash-responses) 
            (setf (gethash h my-hash-responses) (append (gethash h my-hash-responses) (list (second i)))) 
            (setf (gethash h my-hash-responses) (list (second i))))))
    (dotimes (key (/ modelduration minper))  ; TODO this is where I would need to change things. I also added a *1 just to make the warning about modelduration go away
	  ;right now everything that should be "key" is set to "boutNum" TODO kluge locked. Needs to change to allow for actual block index
	  (setf rts (gethash boutNum my-hash-responses))
	  (setf myhist (mapcar #'(lambda (i) (pctless rts i)) humanbins)) ;SCJ assign human data to cumulative bins so that it's in the same format as 
	  ;(print "calculating summaries")
	  (setf (gethash (concatenate 'string "rmse" (write-to-string boutNum)) DVs) (rmse (gethash boutNum targquantiles) myhist))
	  (setf rms (gethash (concatenate 'string "rmse" (write-to-string boutNum)) DVs))
	  (setf alert (mapcan #'(lambda (i) (if (and (> i 150) (< i 500)) (list i))) rts))
      (setf lapse (mapcan #'(lambda (i) (if (and (< i 30000) (> i 500)) (list i))) rts))
      (setf fs (mapcan #'(lambda (i) (if (< i 150) (list i))) rts))
      (setf sa (mapcan #'(lambda (i) (if (= i 30000) (list i))) rts))
	  ;(print "filtering / printing things")		  
	  (setf (gethash (concatenate 'string "mean" (write-to-string boutNum)) DVs) (if (> (length alert) 0) (round (mean alert) 1) -10000))
      (setf (gethash (concatenate 'string "median" (write-to-string boutNum)) DVs) (if (> (length alert) 0) (round (median alert) 1) -10000))
      (setf (gethash (concatenate 'string "lapse" (write-to-string boutNum)) DVs) (if (> (length rts) 0) (/ (float (length lapse))  (length rts)) -10000))
      (setf (gethash (concatenate 'string "fs" (write-to-string boutNum)) DVs) (if (> (length rts) 0) (/ (float (length fs))  (length rts)) -10000))
      (setf (gethash (concatenate 'string "sa" (write-to-string boutNum)) DVs) (if (> (length rts) 0) (/ (float (length sa))  (length rts)) -10000));(list "subjects" "day" "hour" "min" "fpbmc" "fpmc" "utbmc" "utmc" "dat" "ut0" "iu" "egs" "meanalertRT" "medianalertRT" "lapsespct" "fapct" "sapct" "numtrials")
	  ;fa or fs is False Starts and SA is sleep attacks
      (if *local* (write-list-to-file (list  subjects (floor (/ *hour* 24)) (mod *hour* 24) boutNum (no-output (sgp :fpbmc :fpmc :utbmc :utmc :dat :ut0 :iu :egs)) (if (> (length alert) 0) (round (mean alert) 1) "") (if (> (length alert) 0) (round (median alert) 1) "") (/ (float (length lapse))  (length rts)) (/ (float (length fs))  (length rts)) (/ (float (length sa))  (length rts)) (length rts) rms) *blocksummary*))
	 )
	DVs
	
))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; ACT-R Code
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (declare (ignorable key))
  (setf *response*
    (if *actr-enabled-p*
				(progn
					(get-time))
				(get-internal-real-time)))
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Model Code -- Psychomotor Vigilance Task (PVT) -- ACT-R 6.0
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; Author      : Glenn Gunzelmann, Bella Z. Veksler
;;; Copyright   : (c)2016 AFRL/HEAS, All Rights Reserved
;;; Availability: tbd
;;; Address     : AFRL/HEAS
;;;             : 6030 South Kent St.
;;;             : Mesa, AZ  85212-6061
;;;             : glenn.gunzelmann@mesa.afmc.af.mil
;;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; History
;;;
;;;  2004-07-21 - Glenn Gunzelmann
;;;             : Created from old combined task/model code file
;;;
;;;  2007-12-10 - Glenn Gunzelmann
;;;             : Completed careful validation of appropriate behavior
;;;
;;;  2016-10-28 - Bella Z. Veksler
;;;             : Included eval statements for writing out trace of productions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Initialization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *actr-enabled-p* t)

(clear-all)

(define-model fatigue-pvt

  (sgp-fct (list
            :iu *iu* 

            :fpbmc *fpbmc*
            :fpmc *fpmc* 
            
            :utbmc *utbmc*
            :utmc *utmc*
            :ut0 *ut0*

            :hour *hour*            
            :dat *dat*
            :fp-dec *fp-dec*
			
			
			:egs *egs* ;SCJ added because we expected EGS to be a possible mechanism of fatigue action
			:fdbmc 0   ;SCJ added because we want to turn off fdbmc - do we even suspect a biomath effect on declarative? Not from Walsh, we don't?
			:fd-dec 1  ;SCJ added because declarative shouldn't be set to 0...
            ))
  (sgp-fct (list :ppm (* (car (no-output (sgp :iu))) 2)))

(sgp :er t 
     :esc t 
     :ol nil
     :v nil  
     :trace-detail low ;SCJ changed from high because I trust the pvt model and don't need to change it
     :randomize-time t
     :UNSTUFF-VISUAL-LOCATION nil    ;;required for ACT-R 7
     :vpft t  ;; this applies the randomize-time function to cognitive cycle times
     :ncnar nil ;;controls whether or not the system makes chunks "look" pretty after a run by cleaning up the merged names
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following are set to t above.
;;
;; (pm-set-params :real-time t)
;;   -Causes model to run in real-time
;;
;; (pm-set-params :show-focus t)
;;   -Will show the current location of the eye's fixation
;;
;; (sgp :v t)
;;   -Will print the model trace to the active listener
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Chunk-types and declarative memory
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(chunk-type do-pvt state)

(add-dm (trial isa do-pvt state wait)
        (wait isa chunk)
        (look isa chunk)
        (done isa chunk))

(goal-focus trial)
(set-similarities (wait look -.5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Productions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(P wait
   =goal>
      ISA                   do-pvt
      state                 wait
    - state                 done
   ?manual>
      state                 free
   ?visual>
      state                 free
   ?visual-location>
      buffer                empty
   ==>  
!eval! (write-trace 'wait *trace* 0)
 )


(P attend
   =goal>
      ISA                   do-pvt
      state                 =state
    - state                 done                       
    - state                 look
   =visual-location>
      ISA                   visual-location
   ==>
   +visual>
      ISA                   move-attention
      screen-pos            =visual-location
   =goal>
      state                 look  
 
!eval! (write-trace 'attend *trace* 0)
)


(P respond
   =goal>
      ISA                   do-pvt
      state                 look
    - state                 done
   ?manual>
      state                 free
   ?visual>
      state                 free
==>
   +manual>
      ISA    punch
      hand   right
      finger index
   =goal>
      state                 done
   +visual>   ISA           clear 
!eval! (write-trace 'respond *trace* 0)   
)
)