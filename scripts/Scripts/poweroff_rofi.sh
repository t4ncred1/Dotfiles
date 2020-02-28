#!/bin/bash

if [ -z $@ ] 
then
    
    echo shutdown
    echo reboot

else

    OPERATION=$@

    if [ x"shutdown" = x"${OPERATION}" ] 
    then
        shutdown now;
    
    elif [ x"reboot" = x"${OPERATION}" ] 
    then
        reboot;
    
    fi

fi
