(in-package #:tracer)

;;; Trace operations:
;;; 1. Reset
;;; 2. Trace
;;; 2.5 snapshot tracing?
;;; 3. Stop tracing
;;; 4. Save report

#-sbcl (error "This system currently works only on SBCL.")

(defvar *tracing-p* nil "Is currently tracing activity happening?")

;;; Trace info entry type, for function call
;;; - Timestamp
;;; - Function name
;;; - Function args maybe? (trace-with-args), on enter
;;; - Function return value, on exit
;;; - Beginning or ending
;;; - Thread ID



;;; This prints a representation of the return values delivered.
;;; First, this checks to see that cookie is at the top of
;;; *TRACED-ENTRIES*; if it is not, then we need to adjust this list
;;; to determine the correct indentation for output. We then check to
;;; see whether the function is still traced and that the condition
;;; succeeded before printing anything.

(defmacro with-tracing ((&rest specs) &body body)
  (let ((trace-gensyms (map-into (copy-list specs) #'gensym)))
    `(let ,(mapcar (lambda (alias original) `(,alias (fdefinition ',original))) trace-gensyms specs)
       (unwind-protect
            (progn
              ,@(mapcar (lambda (function) `(setf (fdefinition ',function) (wrap-with-tracing (function ,function) ',function)))
                        specs)
              (progn ,@body))
         ,@(mapcar (lambda (function gensym) `(setf (fdefinition ',function) ,gensym))
                   specs
                   trace-gensyms)))))

;;; FIXME: this still has an SBCL dependency -- Jacek Złydach, 2019-10-18
(defun function-name->name-and-category (function-name)
  (etypecase function-name
    (symbol
     (values (symbol-name function-name) (package-name (symbol-package function-name))))
    (cons
     (ecase (first function-name)
       (setf
        (values (format nil "~S" function-name) (package-name (symbol-package (second function-name)))))
       ((method sb-pcl::combined-method)
        (values (remove #\Newline (format nil "~S" function-name))
                (if (consp (second function-name))
                    (package-name (symbol-package (second (second function-name))))
                    (package-name (symbol-package (second function-name))))))))))

(defgeneric post-process-arg (arg)
  (:method ((arg t))
    "Passthrough method."
    (or (ignore-errors
          (prin1-to-string arg))
        "!!Error printing argument!!"))
  (:documentation "A hook useful for changing the printed representation of input and return values."))

(defmethod post-process-arg ((arg sequence))
  (if (every (lambda (el)  (typep el 'number)) arg)
      (format nil "[~{~F~^, ~}]" (coerce arg 'list))
      (call-next-method)))

;;; FIXME: Something breaks if not collecting args, and :skip-args is NIL. Probably the getf in printing. -- Jacek Złydach, 2019-11-05
(defun trace-event->json (trace-event threads &key (skip-args nil))
  (flet ((sanitize-and-format-args-list (argslist)
           (if skip-args "\"skipped\""
               (substitute #\Space #\Newline (format nil "[~{~S~^, ~}]" (mapcar #'post-process-arg argslist))))))
    (ecase (trace-event-phase trace-event)
      (:enter
       (multiple-value-bind (name category)
           (function-name->name-and-category (trace-event-name trace-event))
         (format nil
                 "{ \"name\" : ~S, \"cat\" : ~S, \"ph\" : \"B\", \"pid\" : 1, \"tid\" : ~D, \"ts\" : ~D, \"args\" : { \"in\" : ~A }}"
                 name
                 category
                 (gethash (trace-event-thread trace-event) threads)
                 (trace-event-timestamp trace-event)
                 (sanitize-and-format-args-list (trace-event-args trace-event)))))
      (:exit
       (multiple-value-bind (name category)
           (function-name->name-and-category (trace-event-name trace-event))
         (format nil
                 "{ \"name\" : ~S, \"cat\" : ~S, \"ph\" : \"E\", \"pid\" : 1, \"tid\" : ~D, \"ts\" : ~D, \"args\" : { \"out\" : ~A }}"
                 name
                 category
                 (gethash (trace-event-thread trace-event) threads)
                 (trace-event-timestamp trace-event)
                 (sanitize-and-format-args-list (trace-event-args trace-event)))))
      (:complete
       (multiple-value-bind (name category)
           (function-name->name-and-category (trace-event-name trace-event))
         (format nil
                 "{ \"name\" : ~S, \"cat\" : ~S, \"ph\" : \"X\", \"pid\" : 1, \"tid\" : ~D, \"ts\" : ~D, \"dur\" : ~D,  \"args\" : { \"in\" : ~A, \"out\" : ~A }}"
                 name
                 category
                 (gethash (trace-event-thread trace-event) threads)
                 (trace-event-timestamp trace-event)
                 (trace-event-duration trace-event)
                 (sanitize-and-format-args-list (getf (trace-event-args trace-event) :in))
                 (sanitize-and-format-args-list (getf (trace-event-args trace-event) :out))))))))

(defun thread->json (thread threads)
  (format nil
          "{ \"name\" : \"thread_name\", \"ph\" : \"M\", \"pid\" : 1, \"tid\" : ~D, \"args\" : { \"name\" : ~S }}"
          (gethash thread threads)
          (bt2:thread-name thread)))

(defun enumerate-threads (events)
  (loop
    :with uniques-ht := (make-hash-table :test #'eq)
    :with numbering := 0
    :for event :in events
    :do
       (unless #1=(gethash (trace-event-thread event) uniques-ht)
         (setf #1# (incf numbering)))
    finally
       (return uniques-ht)))

(defun extract-threads (threads)
  (alexandria:hash-table-keys threads))

;;; FIXME: save with streams instead? -- Jacek Złydach, 2019-10-14
(defun save-report (output-file-name &key (skip-args t))
  (let ((threads (enumerate-threads *trace-events*)))
    (with-open-file (stream output-file-name :direction :output :if-exists :supersede)
      ;; TODO: preamble -- Jacek Złydach, 2019-10-14
      (format stream "{~%\"traceEvents\" :  [~%")
      (loop
        for (entry . restp) on *trace-events*
        do
           (write-string (trace-event->json entry threads :skip-args skip-args) stream)
           (when restp
             (write-string "," stream)
             (terpri stream)))
      (loop
        for (thread . restp) on (extract-threads threads)
        initially
           (write-string "," stream)
           (terpri stream)
        do
           (write-string (thread->json thread threads) stream)
           (when restp
             (write-string "," stream)
             (terpri stream)))

      (format stream "~&],
\"displayTimeUnit\" : \"ms\",
\"application\" : \"FIXME\",
\"version\" : \"FIXME\",
\"traceTime\" : ~S
}"
              " TODO local-time independent time"
              ;;(local-time:format-timestring nil (local-time:now))
              )))
  (values))
