;;;; swm-nmctl.lisp --> Network Nanagement UI for StumpWM

;;; Copyright (C) 2024  Erik P Almaraz <erikalmaraz@fastmail.com>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;; References:
;;; 1. https://config.phundrak.com/stumpwm#networkmanager-integration
;;; 2. https://github.com/stumpwm/stumpwm-contrib/tree/master/util/stumpwm-nm


(defpackage :swm-nmctl
  (:use :cl
        :stumpwm)
  (:import-from :foo/baz
                #:sym1
                #:sym2)
  (:local-nicknames (#:nickname :original-package-name))
  (:export #:exsym1))
(in-package :swm-nmctl)


;; TODO - create a stumpwm network manager tool/util for nmcli...
;; say stump-nmcli -> will need a way to prompt for sudo access to establish connections...
