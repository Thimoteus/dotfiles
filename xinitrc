#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then







    xrdb -merge $sysresources

fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then







    xrdb -merge "$userresources"

fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

#session=${1:-xfce}

#case $session in
#  xfce|xfce4) exec startxfce4;;
#  xmonad)
    #nitrogen --restore &
    #xfce4-power-manager &
    #xset m 5/1 0 &
    #xsetroot -cursor_name left_ptr &
    #unclutter &
    #setxkbmap -option caps:swapescape &
    #exec xmonad;;
#  *) exec $1;;
#esac

#nitrogen --restore &
#feh --randomize --bg-fill ~/Pictures/wallpapers/* &
urxvtd -q -f -o
feh --bg-fill ~/Pictures/wallpapers/trigomnometry.jpg &
xfce4-power-manager &
xset m 5/1 0 &
xsetroot -cursor_name left_ptr &
unclutter &
setxkbmap -option caps:swapescape &
compton &
redshift &
exec xmonad
