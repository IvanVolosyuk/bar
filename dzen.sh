#!/bin/bash

cd ~/.xmonad/ivan
export PATH=$(dirname $0):$PATH
bar >&$HOME/bar.log
#dzen | dzen2 -bg "#BEBEBE" -fg black -h 24 -ta l
