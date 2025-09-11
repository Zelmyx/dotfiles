#!/bin/bash

entries="⇠ Logout\n🔒 Lock\n🌙 Suspend\n⭮ Reboot\n⏻ Shutdown"

selected=$(echo -e $entries|wofi --width 250 --height 240 --dmenu --cache-file /dev/null | awk '{print tolower($2)}')

case $selected in
    logout)
        pkill -u edvin;;
    lock)
        exec /home/edvin/.config/sway/lock_screen.sh;;
    suspend)
        swaylock -i /home/edvin/.config/sway/wallpaper.jpg & sleep 0.5 && systemctl suspend;;
    reboot)
        exec systemctl reboot;;
    shutdown)
        exec systemctl poweroff;;
        # it used to be poweroff -i
esac
