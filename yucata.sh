#!/bin/bash

n=$(./yucata-tools/yucata.py | perl -ne '/you can move at (\d+) games/; print $1')

if [ "$n" -ne 0 ]
then
    echo "<spaceout><icon>pawn</icon><thintext>$n</thintext><icon>pawn</icon></spaceout>"
fi
