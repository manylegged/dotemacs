#!/bin/sh

#git $1
if [ `uname` = "Darwin" ]; then
	SPATH="$HOME/Library/Application Support/Sublime Text/Packages"
elif [ `uname` = "Linux" ]; then
	SPATH="$HOME/.config/sublime-text/Packages"
	VPATH="$HOME/.config/Code/User"
else
	SPATH="`cygpath $APPDATA`/Sublime Text/Packages"
	VPATH="`cygpath $APPDATA`/Code/User"
fi

PACKAGES="User C++ Rainglow Clojure"

if [ "$1" = "pull" ]; then
	for pkg in $PACKAGES; do
    	cp -vr $pkg "$SPATH/"
	done
    mkdir -p "$VPATH"
    cp -v ../vscode/* "$VPATH/"
elif [ "$1" = "push" ]; then
	for pkg in $PACKAGES; do
        cp -vr "$SPATH/$pkg" .
	done
    cp -v $VPATH/*.json ../vscode/
else
    echo "push or pull"
fi

