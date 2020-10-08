#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar > /dev/null; do sleep 1; done

desktop=$(wmctrl -m | awk 'NR==1 {print $2}')

case $desktop in
    "bspwm") polybar mainbar-bspwm & ;;
    "herbstluftwm") polybar mainbar-herbstluftwm & ;;
    "i3") polybar mainbar-i3 & ;;
    "sway") polybar mainbar-i3 & ;;
    * ) echo "No WM found" ;;
esac
