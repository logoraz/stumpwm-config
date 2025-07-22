;;;; swm-nmctl.asd

(defsystem "swm-nmctl"
  :description "Provides simple interface to connect to networks via nmcli"
  :author "Erik P Almaraz"
  :license "GPLv3"
  :version "0.1.1"
  :depends-on ("stumpwm")
  :components ((:file "swm-nmctl")))
