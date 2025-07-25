(defpackage :swm-config/utilities
  (:use :cl
        :stumpwm))
(in-package :swm-config/utilities)


;;; Experimental stumpwm-contrib packages (to trial)
(notify:notify-server-toggle)
(setf notify:*notify-server-max-title-lines* 2
      notify:*notify-server-max-body-lines* 20
      notify:*notify-server-max-line-length* 100
      notify:*notify-server-title-color* "^B^6"
      notify:*notify-server-body-color* "^7")

;; (defun notification-handler (app icon summary body)
;;   "Does things with incoming notifications"
;;   ...)

;; You can test notifications (if you have notify-send installed) with:
;; notify-send 'Hello world!' 'This is an example notification.'

(defun save-log-file (pathspec output)
  "Save log files for initializing lem-config"
  (with-open-file (strm (uiop:xdg-config-home pathspec)
                        :direction :output
                        :if-exists :append
                        :if-does-not-exist :create)
    (format strm "~A - Load swm-config output: ~A~%" (current-time) output)))

(multiple-value-bind (result error-condition)
    (ignore-errors
      (when *initializing*
        (mode-line)))
  (if error-condition
      (save-log-file "stumpwm/modeline-error.log" error-condition)))

;;; Notify that everything is ready!
(setf *startup-message* (concat "^6*^BGreetings logoraz! "
                                "Your StumpWM session is ready...^b"))
