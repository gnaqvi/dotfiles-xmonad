# Read .Xresources
xrdb -merge ~/.Xresources &

# Set pointer to left pointer
xsetroot -cursor_name left_ptr &

# Set background
feh-bg

# Start stalonetray
stalonetray &

# Start NetworkManager applet
if [ -x /usr/bin/nm-applet ] ; then
   nm-applet --sm-disable &
fi

# Start app daemons
dropboxd &
mpd
pidgin &
udiskie --tray &

# Start screensaver
xss-lock -- xscreensaver-command -lock &

# Start Compton composting manager
compton --config ~/.compton.conf -b

# Start xmonad
exec xmonad
