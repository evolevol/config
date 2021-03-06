#!/bin/bash
source $HOME/.xmonad/scripts/theme
#Layout
WIDTH=$DATE_WIDTH
X_POS=1150
#X_POS=$DATE_X_POS

printDate() {
        echo -n "$(date '+%Y.^fg()%m.^fg()%d^fg(#007b8c) ^fg(#5b6d54)%A ^fg(#a488d9)| ^fg()%H:^fg()%M:^fg()%S ')"
        return
}
printLayout() {
        if [ $KB_Layout == "Rus" ]; then
                Layout_FG="#61a1c1"
        else
                Layout_FG="#60a0c0"
        fi
        echo -n "^fg($Layout_FG)$KB_Layout ^fg()"
        return
}

printBar() {
        while sleep 1; do
                KB_Layout=`skb -1`
                printLayout
                printDate
                echo
        done
        return
}
printBar | dzen2 -x $X_POS -y $Y_POS -w $WIDTH -h $HEIGHT -fn $FONT -ta 'r' -bg $DZEN_BG -fg $DZEN_FG -p -e ''
