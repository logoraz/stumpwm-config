(defsystem "swm-brightness"
  :description "Provides simple commands interface to control and display brightness."
  :author "Erik P Almaraz"
  :license "GPLv3"
  :version "0.0.1"
  :depends-on ("stumpwm"
               "cl-ppcre")
  :components ((:file "swm-brightness")))
