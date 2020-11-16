#!/bin/sh

#git $1

if [ "$1" = "pull" ]; then
    cp -v User/* /cygdrive/c/Users/$USER/AppData/Roaming/Sublime\ Text\ 3/Packages/User/
    mkdir -p /cygdrive/c/Users/$USER/AppData/Roaming/Sublime\ Text\ 3/Packages/C++/
    cp -v C++/* /cygdrive/c/Users/$USER/AppData/Roaming/Sublime\ Text\ 3/Packages/C++/
elif [ "$1" = "push" ]; then
    cp -v /cygdrive/c/Users/$USER/AppData/Roaming/Sublime\ Text\ 3/Packages/User/* User/
    cp -v /cygdrive/c/Users/$USER/AppData/Roaming/Sublime\ Text\ 3/Packages/C++/* C++/
else
    echo "push or pull"
fi

