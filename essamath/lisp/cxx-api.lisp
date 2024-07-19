
(defun api-eval (eval-string)
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
			(return-from api-eval result)
	))
    in-stream-string-rep))))
  