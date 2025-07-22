(defpackage :swm-config/frames
  (:use :cl
        :stumpwm
        :swm-config/theme))
(in-package #:swm-config/frames)

;;; Window Groups & Placement
(when *initializing*
  (grename "HOME")
  (gnewbg  "DEV")
  (gnewbg  "WWW")
  (gnewbg  "ETC")
  (gnewbg  "PRIV"))

(clear-window-placement-rules)

(define-frame-preference "HOME" (nil t t :class "Tiling"))
(define-frame-preference "DEV"  (nil t t :class "Tiling"))
n(define-frame-preference "WWW"  (nil t t :class "Tiling"))
(define-frame-preference "ETC"  (nil t t :class "Tiling"))
(define-frame-preference "PRIV" (nil t t :class "Tiling"))

;; undefined?
;; (setf *dynamic-group-master-split-ratio* 1/2)

;;; X-window settings & stylization via gaps
;; Tell stumpwm to not honor application window size hints
;; This was the cause of swm-gaps crashing with Emacs!
(setf *ignore-wm-inc-hints* t)

;; Window Gaps
(setf swm-gaps:*head-gaps-size*  0
      swm-gaps:*inner-gaps-size* 5
      swm-gaps:*outer-gaps-size* 10)

(when *initializing*
  (swm-gaps:toggle-gaps))

;; Window Stylization Parameters
(setf *maxsize-border-width*      2
      *normal-border-width*       2
      *float-window-border*       2
      *float-window-title-height* 15
      *window-border-style*       :thick
      *window-format*             "%n:%t")

;; (set-win-bg-color        *nord02*)
(set-focus-color         *nord09*)
(set-unfocus-color       *nord03*)
(set-float-focus-color   *nord09*)
(set-float-unfocus-color *nord03*)

;;; Message and Input Windows Stylization Parameters
(set-border-color *nord09*)
(set-fg-color     *nord04*)
(set-bg-color     *nord00*)

(setf *key-seq-color* "^B^6")
(setf *which-key-format* (concat *key-seq-color* "*~5a^n ~a"))

(setf *input-window-gravity*     :top-right
      *message-window-y-margin*  38
      *message-window-margin*    4
      *message-window-padding*   10
      *message-window-y-padding* 10
      *message-window-gravity*   :top-right)


;;; Mouse focus/float window settings
(setf *mouse-focus-policy* :click ; Mouse click should focus the window
      *float-window-modifier* :SUPER) ; Set super key to move floating windows

;;; Navigation
(defcommand delete-window-and-frame () ()
  "Delete the current frame with its window."
  (delete-window)
  (remove-split))

(defcommand hsplit-and-focus () ()
  "Create a new frame on the right and focus it."
  (hsplit)
  (move-focus :right))

(defcommand vsplit-and-focus () ()
  "Create a new frame below and move focus to it."
  (vsplit)
  (move-focus :down))

(defcommand term (&optional program) ()
  "Invoke a terminal, possibly with a @arg{program}."
  (sb-thread:make-thread
   (lambda ()
     (run-shell-command (if program
                            (format nil "kitty ~A" program)
                            "kitty")))))
