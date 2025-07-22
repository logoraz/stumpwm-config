(defpackage :swm-init
  (:use :cl :stumpwm)
  (:import-from :uiop
                :add-package-local-nickname)
  (:documentation "swm-config System Initialization (swm-init.lisp --> config)"))
(in-package :swm-init)


(asdf:initialize-source-registry
 (list :source-registry
       (list :tree (uiop:xdg-config-home "stumpwm/"))
       :inherit-configuration))

(ignore-errors
  ;; A little dirty - may be rough on first run...
  (asdf:load-system :local-time)
  (add-package-local-nickname :lt :local-time))

(defun current-time ()
  "Emits formated time using local-time"
  (lt:format-timestring nil (lt:now)
                        :format '(:year "-" :month "-" :day "-T"
                                  :hour ":" :min ":" :sec)))

(defun save-log-file (pathspec output)
  "Save log files for initializing lem-config"
  (with-open-file (strm (uiop:xdg-config-home pathspec)
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
    (format strm "~A - Load swm-config output: ~A~%" (current-time) output)))

(multiple-value-bind (result error-condition)
    (ignore-errors
     (sb-ext:without-package-locks
       (asdf:load-system :swm-config)))
  (if error-condition
      (save-log-file "stumpwm/error.log" error-condition)
      (save-log-file "stumpwm/startup.log" result)))
