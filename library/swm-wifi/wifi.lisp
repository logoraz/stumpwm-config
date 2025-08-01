(defpackage :wifi
  (:use :cl :common-lisp :stumpwm )
  (:export #:*iw-path*
           #:*wireless-device*
           #:*wifi-modeline-fmt*
           #:*wifi-signal-quality-fmt*
           #:*wifi-signal-quality-fmt-pc*
           #:*use-colors*))
(in-package :wifi)

;; Let's create an implementation in CLOS!
(defclass wifi ()
  ((device-name :accessor device-name :initarg :device-name))
  (:documentation "The foundation of all wifi devices."))


(defvar *iw-path* "/usr/bin/iw"
  "Location of iw, defaults to /usr/bin/iw.")

(defvar *wireless-device* nil
  "Set to the name of the wireless device you want to monitor. If set
  to NIL, try to guess.")

(defvar *wifi-modeline-fmt* "%e %p"
  "The default value for displaying WiFi information on the modeline.

@table @asis
@item %%
A literal '%'
@item %e
Network ESSID
@item %p
Signal quality (with percentage sign)
@item %P
Signal quality (without percentage sign)
@end table
")

(defvar *wifi-signal-quality-fmt* "^[~A~D^]"
  "The default formatting of the signal quality")

(defvar *wifi-signal-quality-fmt-pc* "^[~A~D%^]"
  "The default formatting of the signal quality as percentage")

(defvar *use-colors* t
  "Use colors to indicate signal quality.")

(defun sig-quality-fmt (qual)
  (if *use-colors*
      (bar-zone-color qual 80 60 40 t)
      ""))

(defun wifi-get-essid (pair)
  (let ((essid (car pair)))
    (format nil "~A" essid)))

(defun wifi-get-signal-quality-pc (pair)
  (let ((qual (cdr pair)))
    (format nil *wifi-signal-quality-fmt-pc* (sig-quality-fmt qual) qual)))

(defun wifi-get-signal-quality (pair)
  (let ((qual (cdr pair)))
    (format nil *wifi-signal-quality-fmt* (sig-quality-fmt qual) qual)))

(defvar *wifi-formatters-alist*
  '((#\e wifi-get-essid)
    (#\p wifi-get-signal-quality-pc)
    (#\P wifi-get-signal-quality)))

(defmacro defun-cached (name interval arglist &body body)
  "Creates a function that does simple caching. The body must be
written in a functional style - the value returned is set as the
prev-val."
  (let ((prev-time (gensym "PREV-TIME"))
        (prev-val (gensym "PREV-VAL"))
        (now (gensym "NOW")))
    (multiple-value-bind (body decls docstring)
        (alexandria:parse-body body :documentation t)
      `(let ((,prev-time 0)
             (,prev-val "no link"))
         (defun ,name ,arglist
           ,@(when docstring
               (list docstring))
           ,@decls
           (let ((,now (get-internal-real-time)))
             (when (>= (- ,now ,prev-time)
                       (* ,interval internal-time-units-per-second))
               (setf ,prev-time ,now)
               (setf ,prev-val (progn ,@body)))
             ,prev-val))))))

(defun guess-wireless-device ()
  (or (loop
         :for path :in (list-directory "/sys/class/net/")
         :thereis (let ((device-name (car (last (pathname-directory path)))))
                    (if (probe-file (merge-pathnames (make-pathname :directory '(:relative "wireless"))
                                                    path))
                       device-name
                       nil)))
      (error "No wireless device found.")))

(defun-cached fmt-wifi 5 (ml)
  "Formatter for wifi status. Displays the ESSID of the access point
you're connected to as well as the signal strength. When no valid data
is found, just displays nil."
  (declare (ignore ml))
  (block fmt-wifi
    (handler-case
        (let* ((device (or *wireless-device* (guess-wireless-device)))
               (iw (run-shell-command (format nil "~A ~A 2>/dev/null"
                                              *iw-path*
                                              dev)
                                      t))
               (essid (multiple-value-bind (match? sub)
                          (cl-ppcre:scan-to-strings "ESSID:\"(.*)\"" iw)
                        (if match?
                            (aref sub 0)
                            (return-from fmt-wifi "no link"))))
               (qual (multiple-value-bind (match? sub)
                         (cl-ppcre:scan-to-strings "Link Quality=(\\d+)/(\\d+)" iw)
                       (declare (ignorable match?))
                       (truncate (float (* (/ (parse-integer (aref sub 0))
                                              (parse-integer (aref sub 1)))
                                           100))))))
          (format-expand *wifi-formatters-alist*
                         *wifi-modeline-fmt*
                         (cons essid qual)))
      ;; CLISP has annoying newlines in their error messages... Just
      ;; print a string showing our confusion.
      (t (c) (format nil "~A" c)))))

;;; Add mode-line formatter

(add-screen-mode-line-formatter #\I #'fmt-wifi)

;;; EOF
