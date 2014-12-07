#!/bin/bash

n=$(./yucata-tools/yucata.py | perl -ne 'print $1 if /you can move at (\d+) games/')

if [ "$n" -ne 0 ]
then
    echo "<spaceout><icon>pawn</icon><thintext>$n</thintext><icon>pawn</icon></spaceout>"
fi
