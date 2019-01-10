#!/bin/bash


scrot -d 2 -e 'mv $f ~/Images/Shots && notify-send "cheese!" "screenshot saved as $f"'
