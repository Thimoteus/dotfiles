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
#xfce4-power-manager &
feh --bg-fill ~/Pictures/background.jpg &
synclient HorizTwoFingerScroll=1 TapButton1=0 TapButton2=3 TapButton3=2 &
xset m 5/1 0 &
xsetroot -cursor_name left_ptr &
#unclutter &
#setxkbmap -option caps:swapescape &
compton --config ~/dotfiles/compton.conf -b &
touch /tmp/current_control_Speaker &
xbindkeys &
exec xmonad
