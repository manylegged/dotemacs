#!/bin/sh

#git $1
if [ `uname` = "Darwin" ]; then
	SPATH="$HOME/Library/Application Support/Sublime Text/Packages"
elif [ `uname` = "Linux" ]; then
	SPATH="$HOME/.config/sublime-text/Packages"
	VPATH="$HOME/.config/Code/User"
else
	SPATH="/cygdrive/c/Users/$USER/AppData/Roaming/Sublime Text/Packages"
	VPATH="/cygdrive/c/Users/$USER/AppData/Roaming/Code/User"
fi

if [ "$1" = "pull" ]; then
    mkdir -p "$SPATH/User/"
    cp -v User/* "$SPATH/User/"
    mkdir -p "$SPATH/C++/"
    cp -v C++/* "$SPATH/C++/"
    cp -v ../vscode/* "$VPATH/"
elif [ "$1" = "push" ]; then
    cp -v "$SPATH/User/"* User/
    cp -v "$SPATH/C++/"* C++/
    cp -v $VPATH/*.json ../vscode/
else
    echo "push or pull"
fi

