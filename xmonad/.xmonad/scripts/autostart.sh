#!/bin/bash

function run {
  if ! pgrep $1 ;
  then
    $@&
  fi
}

host=$(hostname)

#Find out your monitor name with xrandr or arandr (save and you get this line)

if [ $host == 'umiko' ]
   then
xrandr --output DVI-D-0 --mode 1920x1080 --pos 0x0 --rotate normal --output HDMI-A-1 --primary  --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-A-0 --off
fi

#cursor active at boot
xsetroot -cursor_name left_ptr &

#Some ways to set your wallpaper besides variety or nitrogen
#feh --bg-scale ~/.config/bspwm/wall.jpg &

feh --bg-scale ~/Pictures/Wallpapers\ Anime\ Scenery/43.png &

#starting utility applications at boot time
run variety &
numlockx on &
compton --config $HOME/.config/bspwm/compton.conf &
/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &

dunst > /dev/null 2>&1 &

