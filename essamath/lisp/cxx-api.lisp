
(import 'cl-user::|api-eval|)
(defun cl-user::|api-eval| (eval-string)
  (in-package :maxima)
	(unwind-protect
	(catch 'to-lisp
  (let* (($load_pathname nil)
         (noevalargs nil)
         (in-stream (make-string-input-stream eval-string))
         (stream-truename (get-stream-truename in-stream))
         (in-stream-string-rep
          (if stream-truename
              (setq $load_pathname (cl:namestring stream-truename))
              (format nil "~A" in-stream)))
         (meval-fcn (symbol-function 'meval))
         (expr nil))
    (declare (special *prompt-on-read-hang*))
    (when $loadprint
      (format t (intl:gettext "~&read and interpret ~A~&") in-stream-string-rep))
    (cleanup)
    (let ((*in-stream* in-stream))  ; Bind MAXIMA::IN-STREAM to *IN-STREAM*
      (newline *in-stream*)
      (loop while (and
                    (setq expr (let ((*prompt-on-read-hang* nil)) (mread *in-stream* nil)))
                    (consp expr))
            do 
			(setq form (funcall meval-fcn (third expr)))
			(setq form (nformat-check form))
			(setq result (princ-to-string form))
			(return-from cl-user::|api-eval| result)
	))
    in-stream-string-rep))))

(import 'cl-user::|api-load|)
(defun cl-user::|api-load| (filename)
  (if (or (stringp filename) (symbolp filename) (pathnamep filename))
    (let ((searched-for
  	 ($file_search1 filename
  			'((mlist) $file_search_maxima $file_search_lisp  )))
  	type)
      (setq type ($file_type searched-for))
      (case type
        (($maxima)
         ($batchload searched-for))
        (($lisp $object)
         ;; do something about handling errors
         ;; during loading. Foobar fail act errors.
         (loadfile searched-for t nil))
        (t
         ;; UNREACHABLE MESSAGE: DEFAULT TYPE IS '$OBJECT (SEE $FILE_TYPE BELOW)
         (merror "Maxima bug: Unknown file type ~M" type)))
      searched-for)
    (merror "load: argument must be a string, symbol, or pathname; found: ~M" filename)))
