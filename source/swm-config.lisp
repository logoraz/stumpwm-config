(defpackage :swm-config
  (:use :cl
        :stumpwm)
  (:local-nicknames (#:re :ppcre))
  (:export)
  (:documentation "Initialize X11 settings for StumpWM."))
(in-package :swm-config)


;;; Debugging Logs
;; See stumpwm/primitives.lisp
;; (setf *data-dir* (uiop:xdg-cache-home "stumpwm/"))
;; (stumpwm::ensure-data-dir)

;; (setf *debug-level* 5)
;; (redirect-all-output (data-dir-file "debug-output" "txt"))

;; A startup message can be used when initializing StumpWM, for now set to nil.
(setf *startup-message* nil)

(setf *mouse-focus-policy* :sloppy)

;;;; X11 Settings

;;; Run xmodmap to remap keys
;; (run-shell-command "xmodmap ~/.xmodmap")

;; Set Display/Screen using xrandr
#+or (run-shell-command "xrandr --output eDP --scale 0.6x0.6")

;;; Set pointer/cursor paths & defaults -> theme set in .Xresources
;; Ref |--> https://github.com/ful1e5/XCursor-pro
(run-shell-command "xsetroot -xcf ~/.icons/XCursor-Pro-Dark/cursors/left_ptr 32")

;;; Turn off system bell & screen-saver control
(run-shell-command "xset b off")
(run-shell-command "xset s off")

;;; Disable Touchpad (set here as default)
;; TODO: Refactor Touchpad system - feels ugly...
(defvar *touchpadp* nil
  "Hold boolean state of touchpad.")

;;Note: Similar fn pattern used in swm-brightness & swm-bluetooth.
(defun format-output (value regexp filter exact)
  "Format VALUE according to REGEXP, FILTER, EXACT using ppcre to return desired content."
  (let* ((line-match (re:scan-to-strings regexp value))
         (item-match (re:scan-to-strings filter line-match))
         (content (re:scan-to-strings exact item-match)))
    content))

(defun get-touchpad-id ()
  "Dynamically retrieve touchpad id from xinput"
  (format-output (run-shell-command "xinput list" t)
                 ;; Specific to Framework laptops, need to set regexp accordingly...
                 "(.*(?:Touchpad).*)" ;; matches/captures line w/ Touchpad identifier
                 "(id=[0-9]{2})"      ;; Extract id=## item
                 "([0-9]{2})"))       ;; Extract 2 digit number (after id=##)

(defun set-touchpad-state (&optional (state "disable"))
  "Enable/Disable touchpad."
  (run-shell-command (concat "xinput "
                             "-" state " "
                             (get-touchpad-id))))

(defcommand toggle-touchpad () ()
  "Toggle touchpad control..."
  (set-touchpad-state (if *touchpadp* "disable" "enable"))
  (setf *touchpadp* (not *touchpadp*)))

;; Always start with touchpad disabled
(set-touchpad-state "disable")

;;; ui Settings

;;; Set Wallpaper
(run-shell-command "feh --bg-scale  ~/Pictures/wallpapers/desktop-bg.jpg")

;;; Enable screen locking on suspend
(run-shell-command "xss-lock -- slock")

;;; Additional Xorg resources
(run-shell-command "xrdb -merge ~/.Xresources")

;;; Enable screen compositing
;; Necessary to pecify xrender as backend (transparency/opacity effects)
(run-shell-command "picom --backend xrender")
