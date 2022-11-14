#!/bin/sh

if [ "$1" = "pull" ]; then
    find . -type f | cut -c3- | xargs -I@ cp -v ./@ ~/.@
elif [ "$1" = "push" ]; then
    find . -type f | cut -c3- | xargs -I@ cp -v ~/.@ ./@
else
    echo "push or pull"
fi

