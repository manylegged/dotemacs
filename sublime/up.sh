#!/bin/sh

#git $1
if [ `uname` = "Darwin" ]; then
	SPATH="$HOME/Library/Application Support/Sublime Text 3/Packages"
else
	SPATH="/cygdrive/c/Users/$USER/AppData/Roaming/Sublime Text 3/Packages"
fi

if [ "$1" = "pull" ]; then
	mkdir -p "$SPATH/User/"
    cp -v User/* "$SPATH/User/"
    mkdir -p "$SPATH/C++/"
    cp -v C++/* "$SPATH/C++/"
elif [ "$1" = "push" ]; then
    cp -v "$SPATH/User/"* User/
    cp -v "$SPATH/C++/"* C++/
else
    echo "push or pull"
fi

