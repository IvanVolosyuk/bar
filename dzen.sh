#!/bin/bash 

export PATH=$(dirname $0):$PATH
dzen-info.py  | dzen2 -bg gray -fg black -h 24
