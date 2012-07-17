#!/bin/bash 

cd ~/.xmonad/ivan
export PATH=$(dirname $0):$PATH
#xwidth=$(xdpyinfo  | grep dimensions | cut -d: -f2 | cut -dx -f1)
#dzen-info.py $xwidth | dzen2 -bg gray -fg black -h 24
#~/src/dzen | dzen2 -bg gray -fg black -h 24 -ta l
dzen | dzen2 -bg gray -fg black -h 24 -ta l
