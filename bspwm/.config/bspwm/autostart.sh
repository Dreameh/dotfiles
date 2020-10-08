#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}


# Fix screenlayout
$HOME/.screenlayout/screenlayout.sh &

# Launch polybar
$HOME/.config/polybar/launch.sh &

# Restore wallpapers
nitrogen --restore &
# Running apps
xsetroot -cursor_name left_ptr &
sxhkd &
#run variety &
run discord &
numlockx on &
compton -b &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &
#/usr/lib/xfce4/notifyd/xfce4-notifyd &
dunst > /dev/null 2>&1 &
wal -R
