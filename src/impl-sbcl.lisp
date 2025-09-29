;;;; SBCL-specific implementation of the Tracer.

(in-package #:tracer)

(defvar *trace-events-lock* (bt2:make-lock))
(defvar *trace-events* '() "A list of trace entries, pushed onto from the beginning.")

(defvar *clock-reset-fun* nil)
(defvar *clock-get-time-fun* nil)

(unless (>= internal-time-units-per-second 1000)
  (warn "Tracer clock may not havve enough precision to be useful for profiling use."))

;;; TODO: this needs to be a function that can be changed between invocations of tracing!
;;; I want to allow for injecting higher resolution clocks if available.
;;; -- Jacek Złydach, 2019-11-01

(defun get-current-time-usec ()
  "Get current time with microsecond resolution."
  (* (get-internal-real-time) 1000))

(declaim (inline %get-current-time))
(defun %get-current-time ()
  "Cross-implementation abstraction to get the current time measured from the unix epoch (1/1/1970). Should return (values sec nano-sec)."
  #+(and allegro (not os-windows))
  (flet ((allegro-gettimeofday ()
           (let ((tv (ff:allocate-fobject 'timeval :c)))
             (allegro-ffi-gettimeofday tv 0)
             (let ((sec (ff:fslot-value-typed 'timeval :c tv 'tv_sec))
                   (usec (ff:fslot-value-typed 'timeval :c tv 'tv_usec)))
               (ff:free-fobject tv)
               (values sec usec)))))
    (multiple-value-bind (sec usec) (allegro-gettimeofday)
      (values sec (* 1000 usec))))
  #+(and allegro os-windows)
  (let* ((ft (ff:allocate-fobject 'filetime :c)))
    (allegro-ffi-get-system-time-as-file-time ft)
    (let* ((low (ff:fslot-value-typed 'filetime :c ft '|dwLowDateTime|))
           (high (ff:fslot-value-typed 'filetime :c ft '|dwHighDateTime|)))
      (filetime-to-current-time low high)))
  #+cmu
  (multiple-value-bind (success? sec usec) (unix:unix-gettimeofday)
    (assert success? () "unix:unix-gettimeofday reported failure?!")
    (values sec (* 1000 usec)))
  #+sbcl
  (sb-ext:get-time-of-day)
  #+(and ccl (not windows))
  (ccl:rlet ((tv :timeval))
    (let ((err (ccl:external-call "gettimeofday" :address tv :address (ccl:%null-ptr) :int)))
      (assert (zerop err) nil "gettimeofday failed")
      (values (ccl:pref tv :timeval.tv_sec) (* 1000 (ccl:pref tv :timeval.tv_usec)))))
  #+(and ccl windows)
  (ccl:rlet ((time :<lpfiletime>))
    (ccl:external-call "GetSystemTimeAsFileTime" :<lpfiletime> time :void)
    (let* ((low (ccl:%get-unsigned-long time (/ 0 8)))
           (high (ccl:%get-unsigned-long time (/ 32 8))))
      (filetime-to-current-time low high)))
  #+abcl
  (multiple-value-bind (sec millis)
      (truncate (java:jstatic "currentTimeMillis" "java.lang.System") 1000)
    (values sec (* millis 1000000)))
  #+(and lispworks (or linux darwin))
  (lispworks-gettimeofday)
  #-(or allegro cmu sbcl abcl ccl (and lispworks (or linux darwin)))
  (values (- (get-universal-time)
             ;; CL's get-universal-time uses an epoch of 1/1/1900, so adjust the result to the Unix epoch
             #.(encode-universal-time 0 0 0 1 1 1970 0))
          0))

;;; XXX: below is our new, usec clock -- Jacek Złydach, 2019-11-02
(let ((clock-offset 0))
  (declare (type alexandria:non-negative-fixnum clock-offset))
  (defun %%start-clock ()
    (setf clock-offset (%get-current-time)))
  (defun %%get-time-usec ()
    (multiple-value-bind (sec usec) (%get-current-time)
      (+ (* (- sec clock-offset) 1000000) usec)))
  (defun %%time (thunk)
    (let ((start (%%get-time-usec)))
      (funcall thunk)
      (- (%%get-time-usec)  start)))
  (setf *clock-reset-fun* #'%%start-clock
        *clock-get-time-fun* #'%%get-time-usec))

(declaim (ftype (function () alexandria:non-negative-fixnum) get-current-time)
         (inline get-current-time))
(defun get-current-time ()
  (funcall *clock-get-time-fun*))

(defun post-process-entries (entries &key correct-zero-duration)
  "Destructively modify `ENTRIES', making it more compact and, if `CORRECT-ZERO-DURATION' is T,
changing zero-length events to have 1us length, also modifying other times to offset for that.
`ENTRIES' is expected to be in order entries were added. The function maintain separate offsets per (process, thread) pair.
Returns a processed list, to replace old value `ENTRIES'. As additional values, returns the total accumulated clock offset,
and the stacks containing unclosed duration entries, keyed by thread."
  (let ((offset 0)
        (stacks (make-hash-table :test 'equal)))
    (dolist (entry entries entries)
      ;; Always update event time to account for clock offset.
      (incf (trace-event-timestamp entry) offset)

      ;; Match starting and ending events to offset clock in case of zero-length events, and to convert
      ;; matching pairs of Duration events into Complete events.
      (let ((entry-ht-id (cons 1 (trace-event-thread entry)))) ;1 is the currently supported PID
        (ecase (trace-event-phase entry)
          (:enter
           ;; Push the :enter entry to stack.
           (push entry (gethash entry-ht-id stacks)))
          (:exit
           (let ((begin-event (first (gethash entry-ht-id stacks))))
             (if (equalp (trace-event-name begin-event)
                         (trace-event-name entry))
                 (progn
                   ;; Actual post-processing happens here.
                   ;; If zero-length and correct-zero-duration is on, update close time and offset.
                   (when (and correct-zero-duration
                              (= (trace-event-timestamp begin-event)
                                 (trace-event-timestamp entry)))
                     (incf (trace-event-timestamp entry))
                     (incf offset))

                   ;; Convert task into complete task + counter
                   (setf (trace-event-phase begin-event) :complete
                         (trace-event-phase entry) :skip ;TODO: counters, if any, go here -- Jacek Złydach, 2019-11-04
                         (trace-event-duration begin-event) (- (trace-event-timestamp entry) (trace-event-timestamp begin-event))
                         (trace-event-args begin-event) `(:in ,(trace-event-args begin-event) :out ,(trace-event-args entry)))

                   ;; Pop the updated entry from stack.
                   (pop (gethash entry-ht-id stacks)))
                 (warn "Recorded entries misordered; expected ~A, got ~A." (trace-event-name begin-event) (trace-event-name entry))))))))
    ;; Go over the list again, and remove "skip" entries.
    (alexandria:deletef entries :skip :key #'trace-event-phase)
    (values entries
            offset
            stacks)))

;;; Tracing process

(defmacro tracing-block ((name args) &body body)
  (alexandria:once-only (name args)
    (alexandria:with-gensyms (!current-thread !enter-event !exit-event)
      `(let* ((,!current-thread (bt2:current-thread))
              (,!enter-event (cons (make-trace-event-fast :enter
                                                          ,name
                                                          ,!current-thread
                                                          (get-current-time)
                                                          ,args
                                                          nil
                                                          nil)
                                   nil))
              (result nil))
         (unwind-protect (setf result (values-list (progn ,@body)))
           (let ((,!exit-event (cons (make-trace-event-fast :exit
                                                            ,name
                                                            ,!current-thread
                                                            (get-current-time)
                                                            result
                                                            nil
                                                            nil)
                                     ,!enter-event)))
             (bt2:with-lock-held (*trace-events-lock*)
               (setf (cdr ,!enter-event) *trace-events*
                     *trace-events* ,!exit-event))))))))

(defun wrap-with-tracing (function function-name)
  (lambda (&rest args)
    (tracing-block (function-name args)
      (apply function args))))


(defun get-tracing-report-data ()
  *trace-events*)

(defun reset-tracing-data ()
  (setf *trace-events* '()))
