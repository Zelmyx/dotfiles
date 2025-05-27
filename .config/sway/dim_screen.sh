#!/bin/sh


current_brightness=$(brightnessctl g)
brightnessctl s 0

echo $current_brightness > $HOME/.config/sway/old_brightness
