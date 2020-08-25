;; Org-Babel support for evaluating red source code.

;;; Code:
(require 'ob)
(require 'ob-eval)
(eval-when-compile (require 'cl))

(defvar org-babel-tangle-lang-exts)
(add-to-list 'org-babel-tangle-lang-exts '("red" . "r"))

(defvar org-babel-default-header-args:red nil)

(defcustom org-babel-red-command "red"
"Name of command to use for executing Tcl code."
  :group 'org-babel
  :type 'string)


(defun org-babel-execute:red (body params)
  "Execute a block of Red code with Babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((session (cdr (assoc :session params)))
         (result-params (cdr (assoc :result-params params)))
         (result-type (cdr (assoc :result-type params)))
         (full-body (or body
                        (org-babel-expand-body:generic
		                     body params (org-babel-variable-assignments:red params))))
	       (session (org-babel-red-initiate-session session)))
    (org-babel-red-evaluate session full-body result-type)
    ;; (org-babel-reassemble-table
    ;;  (org-babel-red-evaluate session full-body result-type)
    ;;  (org-babel-pick-name
    ;;   (cdr (assoc :colname-names params)) (cdr (assoc :colnames params)))
    ;;  (org-babel-pick-name
    ;;   (cdr (assoc :rowname-names params)) (cdr (assoc :rownames params))))
    ))

(defun org-babel-prep-session:red (session params)
  "Prepare SESSION according to the header arguments in PARAMS."
  (error "Sessions are not supported for Red"))

(defun org-babel-variable-assignments:red (params)
  "Return list of red statements assigning the block's variables."
  (mapcar
   (lambda (pair)
     (format "set %s %s"
	     (car pair)
	     (org-babel-red-var-to-red (cdr pair))))
   (mapcar #'cdr (org-babel-get-header params :var))))

;; helper functions

(defun org-babel-red-var-to-red (var)
  "Convert an elisp value to a red variable.
The elisp value, VAR, is converted to a string of red source code
specifying a var of the same value."
  (if (listp var)
      (concat "{" (mapconcat #'org-babel-red-var-to-red var "  ") "}")
    (format "%s" var)))

(defvar org-babel-red-buffers '(:default . nil))

(defun org-babel-red-initiate-session (&optional session params)
  "Return nil because sessions are not supported by red."
nil)

(defvar org-babel-red-wrapper-method
  "
%s
")

(defvar org-babel-red-pp-wrapper-method
  nil)

(defun org-babel-red-evaluate (session body &optional result-type)
  "Pass BODY to the Red process in SESSION.
If RESULT-TYPE equals 'output then return a list of the outputs
of the statements in BODY, if RESULT-TYPE equals 'value then
return the value of the last statement in BODY, as elisp."
  (when session (error "Sessions are not supported for Red"))
  (case result-type
    (output (org-babel-eval org-babel-red-command body))
    (value (let* ((tmp-file (org-babel-temp-file "red-"))
                  (code-file (with-temp-file tmp-file (insert body))))

             (with-temp-buffer
               (call-process-shell-command (format "red %s" tmp-file) nil (current-buffer)))
             ;; (org-babel-eval
             ;;  org-babel-red-command
             ;;  (format org-babel-red-wrapper-method body
             ;;          (org-babel-process-file-name tmp-file 'noquote)))
             ;; (org-babel-eval-read-file tmp-file)
             ))))

(provide 'ob-red)



;;; ob-red.el ends here
