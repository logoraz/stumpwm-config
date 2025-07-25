# swm-wifi

A Wifi module implemented in CLOS for StumpWM configured to work with `iw`

## Usage

Place the following in your ~/.stumpwmrc file:

#+BEGIN_SRC lisp
    (load-module "wifi")
#+END_SRC

Then you can use "%I" in your mode line format (both "w" and "W"
were taken. Think _I_EEE 802.11 :-)).

To change the format of the information on the modeline, set
=wifi:*wifi-modeline-fmt*= (default ="%e %p"=).

| Code | Result                                |
|------+---------------------------------------|
| %%   | A literal %                           |
| %e   | Network ESSID                         |
| %p   | Signal quality (with percentage sign) |
| %P   | Signal quality (without percentage)   |

To enable/disable colors in the signal quality indicator, set
=wifi:*use-colors*= (default =t=).

## Notes
This gets information through sysfs, so it only works on Linux with a
mounted =sysfs=.

## WARNING

This triggers a yet to be discovered bug in SBCL, which causes
stumpwm to freeze.
 

<2014-03-16 Sun> This is a fairly old file, maybe the bug was fixed?
Can someone confirm?

<2017-08-11 Fri> I've had this running all day and not had any problems yet:
Debian Jessie 8.8 x86_64
SBCL 1.3.17
stumpwm 1.0.0-111-gb740b04
