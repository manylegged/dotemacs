#!/bin/sh

if [ "$1" = "pull" ]; then
    cp -v User/* /cygdrive/c/Users/$USER/AppData/Roaming/Sublime\ Text\ 3/Packages/User/
elif [ "$1" = "push" ]; then
    cp -v /cygdrive/c/Users/$USER/AppData/Roaming/Sublime\ Text\ 3/Packages/User/* User/
else
    echo "push or pull"
fi

