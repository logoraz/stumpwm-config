#!/bin/sh

# Launch xterm in the background
xterm &

# Sleep long enough to get the window open, and set transparency
sleep 0.15s
transset-df -a 0.90
