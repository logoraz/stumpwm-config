(asdf:defsystem #:swm-wifi
  :description "Display wifi information."
  :author "Erik Almaraz"
  :license "MIT"
  :depends-on (#:stumpwm
               #:alexandria)
  :components ((:file "wifi")))
