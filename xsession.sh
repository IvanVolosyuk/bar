#!/bin/bash

$(dirname $0)/tray.sh &
exec /home/vol/opt/bin/xmonad
