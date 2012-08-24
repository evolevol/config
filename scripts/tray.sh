#!/usr/bin/env bash
source $HOME/.xmonad/scripts/theme
WIDTH=50
HEIGHT=22
MARGIN=$TRAY_MARGIN
#trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true --widthtype pixel --width $WIDTH --heighttype pixel --height $HEIGHT --margin $MARGIN --transparent true --alpha 0 --tint 0x1a1a1a

trayer --edge bottom --align right --SetDockType true --expand true --width 13 --transparent true --tint 0x1a1a1a --alpha 0 --distance 0 --padding 0 --heighttype pixel --height 18
