(defsystem "swm-config"
  :description "StumpWM config with batteries included!"
  :author "Erik P Almaraz <erikalmaraz@fastmail.com"
  :license "MIT"
  :version (:read-file-form "version.sexp" :at (0 1))
  :depends-on ("stumpwm"
               "slynk"
               "micros"
               ;; stumpwm-contrib modules
               "ttf-fonts"
               "swm-gaps"
               ;; "kbd-layouts"
               "globalwindows"
               "cpu"
               "mem"
               "battery-portable"
               "wifi"
               "notify"
               ;; Local Systems/Libraries (modified stumpwm-contrib modules)
               "swm-wpctl"
               "swm-end-session"
               "swm-screenshot"
               "swm-bluetooth"
               "swm-brightness")
  :components ;; Map of System
  ((:module "source"
    :serial t
    :components
    ((:file "swm-config")
     (:file "syntax")
     (:file "theme")
     (:file "frames")
     (:file "keybindings")
     (:file "modeline")
     (:file "utilities")))))

(register-system-packages "bordeaux-threads" '(:bt :bt2 :bordeaux-threads-2))
