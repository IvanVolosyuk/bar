#!/bin/bash

cd ~/.xmonad/ivan
export PATH=$(dirname $0):$PATH
dzen | dzen2 -bg gray -fg black -h 24 -ta l
