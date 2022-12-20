#!/bin/sh
/opt/lispnyc/robocup-stop.sh
xterm -bg grey20 -title "Robocup Server" -e /opt/robocup/bin/rcssserver -coach -sfile /home/kenny/rc/trainer.conf &
/opt/robocup/bin/rcssmonitor &
