(defpackage :swm-config/modeline
  (:use :cl
        :stumpwm
        :swm-config/theme)
  (:import-from :alexandria
                #:xor))
(in-package :swm-config/modeline)


;; The timeout of the modeline indicates how often it refreshes in seconds.
(setf *mode-line-timeout* 2)

;; Formatting options
(setf *time-modeline-string* "%F %I:%M:%S")

;; Let’s also indicate how the groupname is displayed.
(setf *group-format* "%t")

;; The window format should display first its window number, then title -
;; limited to 30 characters.
(setf *window-format* " %n: %30t ")

;; Set mode-line colors
(setf *mode-line-background-color* *nord00*
      *mode-line-foreground-color* *nord04*)

(setf *mode-line-border-color* *nord03*
      *mode-line-border-width* 1)

;;; Configure parameters for mode-line modules 
;; f0 = Hack, f1 = JetBrains Mono, f2 = Symbols Nerd Font Mono, f3 = FontAwesome
(setf cpu::*cpu-modeline-fmt*        "%c %t"
      cpu::*cpu-usage-modeline-fmt*  "^f2^f0 ^[~A~0D%^]"

      mem::*mem-modeline-fmt*        " ^f2^f0%p"

      swm-wpctl:*wpctl-path*         "/usr/bin/wpctl"
      swm-wpctl:*mixer-command*      "/usr/bin/playerctl"
      swm-wpctl:*modeline-fmt*       "^f2󱄠^f0 %v"

      ;; TODO Create new module to handle both `wi' and `wiconfig'
      ;; wifi::*iwconfig-path*          "/usr/bin/iw"
      ;; wifi::*wifi-modeline-fmt*      "^f2^f0 %e %p"

      ;; mode-line formatters
      *hidden-window-color*          "^**"
      *mode-line-highlight-template* "^[^97^R~A^r^]")


;;; Modeline Formatter
(defparameter +L+ "^f2^f0")
(defparameter +B+ "^f2󰄌^f0 %B")

;;TODO - Investigate why CPU is no longer working... Fix wifi module to work
;;       on fedora
(defvar *mode-line-formatter-list*
  `(("%g")    ;; Groups
    ("%W")    ;; Windows
    ("^>")    ;; StumpWM modeline seperator
    (,+L+)    ;; Common Lisp Logo o.O
    ;; ("mu-unread" . t) ;; example of supply shell command
    ;; ("%C %M") ;; CPU usage & Memory usage
    ("%P")    ;; Audio info
    ;; ("%I")    ;; Wifi status
    (,+B+)    ;; Battery info
    ("%d"))   ;; Date/Time
  "List of formatters for the modeline.")

;;; ref: https://config.phundrak.com/stumpwm/
(defun generate-modeline (elements &optional not-invertedp rightp)
  "Generate a 'Powerline' style modeline for StumpWM.

ELEMENTS should be a list of cons cells where `car` is the modeline
formatter or the shell command to run, and their `cdr` is either nil
when the `car` is a formatter and t when it is a shell command."
  (when elements
    (cons (format nil " ^[~A^]^(:bg \"~A\") "
                  (format nil "^(:fg \"~A\")^(:bg \"~A\")~A"
                          (if (xor not-invertedp rightp) *nord00* *nord02*)
                          (if (xor not-invertedp rightp) *nord02* *nord00*)
                          (if rightp "" ""))
                  (if not-invertedp *nord02* *nord00*))
          (let* ((current-element (first elements))
                 (formatter       (car current-element))
                 (commandp        (cdr current-element)))
            (cons (if commandp
                      `(:eval (run-shell-command ,formatter t))
                      (format nil "~A" formatter))
                  (generate-modeline (rest elements)
                                     (not not-invertedp)
                                     (if (string= "^>" (car (first elements))) t 
                                         rightp)))))))

(defcommand reload-modeline () ()
  "Reload modeline."
  (sb-thread:make-thread
   (lambda ()
     (setf *screen-mode-line-format*
           (rest (generate-modeline *mode-line-formatter-list*))))))

;; Load newly designed mode-line
(reload-modeline)

;;; start the mode line
#+nil
(when *initializing*
  (mode-line))
