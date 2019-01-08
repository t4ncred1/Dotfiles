#!/usr/bin/bash

KEYBOARD_LAYOUT=$(setxkbmap -query | awk 'FNR == 3 {print $2}')

if [[ "$KEYBOARD_LAYOUT" == "us" ]]
then
    setxkbmap it
else 
    setxkbmap us
fi
