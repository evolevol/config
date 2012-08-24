#!/usr/bin/env bash
source $HOME/.xmonad/scripts/theme
#Layout
BAR_H=9
BIGBAR_W=50
SMABAR_W=15

X_POS=$TOPSTATUSBAR_X_POS
WIDTH=1150
#WIDTH=$TOPSTATUSBAR_WIDTH

#Options
IFS='|' #internal field separator (conky)
CONKYFILE="/home/evol/.xmonad/scripts/conkyrc"
ICONPATH="/home/evol/.xmonad/icons/subtlexbm"
INTERVAL=1
CPUTemp=0
GPUTemp=0
CPULoad0=0
CPULoad1=0
CPUFan=0
NetUp=0
NetDown=0

printBacklightInfo() {
        backlight=$(cat /sys/class/backlight/acpi_video0/brightness)
        if [[ $backlight == "0" ]]; then
                #echo -n "^fg($COLOR_ICON)^i($ICONPATH/light1.xbm)^fg() "
		echo -n "^fg()${backlight}"
        else
                #echo -n "^fg($COLOR_ICON)^i($ICONPATH/light2.xbm)^fg() "
		echo -n "^fg()${backlight}"
        fi
        return
}

printVolInfo() {
	Perc=$(amixer get Master | grep "Mono:" | awk '{print $4}' | tr -d '[]%')
	Mute=$(amixer get Master | grep "Mono:" | awk '{print $6}')
	if [[ $Mute == "[off]" ]]; then
		#echo -n "^fg($COLOR_ICON)^i($ICONPATH/volume_off.xbm) "
		echo -n "^fg()off "
		echo -n "$(echo $Perc | gdbar -fg $CRIT -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -nonl)"
	else
		#echo -n "^fg($COLOR_ICON)^i($ICONPATH/volume_on.xbm) "
		echo -n "^fg()${Perc}% "
		echo -n "$(echo $Perc | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -nonl)"
	fi
	return
}

printCPUInfo() {
        #echo -n " ^fg($COLOR_ICON)^i($ICONPATH/cpu.xbm)^fg() "
        echo -n "$(echo $CPULoad0 | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $SMABAR_W  -nonl)"
        echo -n "$(echo $CPULoad1 | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $SMABAR_W  -nonl)"
        echo -n "$(echo $CPULoad2 | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $SMABAR_W  -nonl)"
        echo -n "$(echo $CPULoad3 | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $SMABAR_W  -nonl) "
        echo -n "${CPUFreq}GHz"
        return
}

printTempInfo() {
        #CPUTemp=$(acpi --thermal | awk '{print substr($4,1,2)}')
        #CPUTemp0=$(cat /sys/class/hwmon/hwmon2/device/temp2_input | cut -b1-2)
        #CPUTemp2=$(cat /sys/class/hwmon/hwmon2/device/temp4_input | cut -b1-2)
        CPUTemp=$(cat /sys/class/hwmon/hwmon0/temp1_input | cut -b1-2)
        CPUTemp0=$(cat /sys/class/hwmon/hwmon1/device/temp2_input | cut -b1-2)
        CPUTemp2=$(cat /sys/class/hwmon/hwmon1/device/temp4_input | cut -b1-2)
	CPUfanlevelspeed=$(cat /proc/acpi/ibm/fan | grep "level:" | awk '{print $2}')
	if [[ $CPUTemp -gt 70 ]]; then
                 CPUTemp0="^fg($CRIT)$CPUTemp0^fg()"
                 CPUTemp2="^fg($CRIT)$CPUTemp2^fg()"
                 CPUTemp="^fg($CRIT)$CPUTemp^fg()"
        fi
        echo -n "^fg($DZEN_FG2) ^fg()${CPUTemp0}C"
        echo -n "^fg($DZEN_FG2) ^fg()${CPUTemp2}C"
        echo -n "^fg($DZEN_FG2) ^fg()${CPUTemp}C"
        echo -n "^fg($DZEN_FG2) ^fg()${CPUfanlevelspeed}"
        return
}

printMemInfo() {
        #echo -n "^fg($COLOR_ICON)^i($ICONPATH/memory.xbm) "
        echo -n "^fg()${MemUsed} "
        echo -n "$(echo $MemPerc | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $SMABAR_W -nonl)"
        return
}

printBattery() {
        BatPresent=$(acpi -b | wc -l)
        ACPresent=$(acpi -a | grep -c on-line)
        #if [[ $ACPresent == "1" ]]; then
        #        echo -n "^fg($COLOR_ICON)^i($ICONPATH/ac1.xbm) "
        #else
        #        echo -n "^fg($COLOR_ICON)^i($ICONPATH/battery_vert3.xbm) "
        #fi
        if [[ $BatPresent == "0" ]]; then
                echo -n "^fg($DZEN_FG2)AC ^fg()on ^fg($DZEN_FG2)Bat ^fg()off"
                return
        else
                RPERC=$(acpi -b | awk '{print $4}' | tr -d "%,")
		RPTIME=$(acpi -b | awk '{print $5}')
                echo -n "^fg()$RPERC% "
		echo -n "$RPTIME "
                if [[ $ACPresent == "1" ]]; then
                        echo -n "$(echo $RPERC  | gdbar -fg $BAR_FG -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -ss 1 -sw 4 -nonl)"
                else
                        echo -n "$(echo $RPERC  | gdbar -fg $CRIT -bg $BAR_BG -h $BAR_H -w $BIGBAR_W -ss 1 -sw 4 -nonl)"
                fi
        fi
        return
}

printMPDinfo() {
	if [ ! `mpc | grep -o "\[.*\]"` == "[paused]" ]
	then
		mpc_current=$(mpc current | sed 's/AnimeNfo Radio | Serving you the best Anime music!: //')
		mpd="${mpc_current}"
		#echo -n "^fg($COLOR_ICON)^i($ICONPATH/musicS.xbm)^fg() "
		echo -n "^fg($DZEN_FG2)$mpd"
	else
		mpd=""
	fi
}


printMail() {
        echo -n "^fg($COLOR_ICON)^i($ICONPATH/mail.xbm) ^fg()${GMAIL}"
        return
}

printDiskInfo() {
	RFSP=$(df -h / | tail -1 | awk -F' ' '{ print $4 }' | sed s/G//)
	RFSP_M=$(df -m / | tail -1 | awk -F' ' '{ print $4 }')
	BFSP=$(df -h /home | tail -1 | awk -F' ' '{ print $4 }' | sed s/G//)
	BFSP_M=$(df -m /home | tail -1 | awk -F' ' '{ print $4 }')
	hddBFSP=$(df -h /media/hdd | tail -1 | awk -F' ' '{ print $4 }' | sed s/G//)
	hddBFSP_M=$(df -m /media/hdd | tail -1 | awk -F' ' '{ print $4 }')
	#echo -n "^fg($COLOR_ICON)^i($ICONPATH/file1.xbm) "
	if [[ $RFSP_M -le 1024 ]]; then
		RFSP=$(df -h / | tail -1 | awk -F' ' '{ print $4 }')
		RFSP="^fg($CRIT)"$RFSP"^fg()"
		echo -n "^fg($DZEN_FG2)root ^fg()${RFSP} "
	else echo -n "^fg($DZEN_FG2)root ^fg()${RFSP}G "
	fi
	if [[ $BFSP_M -le 1024 ]]; then
		BFSP=$(df -h /home | tail -1 | awk -F' ' '{ print $4 }')
		BFSP="^fg($CRIT)"$BFSP"^fg()"
		echo -n "^fg($DZEN_FG2)home ^fg()${BFSP} "
	else echo -n "^fg($DZEN_FG2)home ^fg()${BFSP}G "
	fi
	if [[ $hddBFSP_M -le 1024 ]]; then
		hddBFSP=$(df -h /media/hdd | tail -1 | awk -F' ' '{ print $4 }')
		hddBFSP="^fg($CRIT)"$hddBFSP"^fg()"
		echo -n "^fg($DZEN_FG2)hdd ^fg()${hddBFSP}"
	else echo -n "^fg($DZEN_FG2)hdd ^fg()${hddBFSP}G "
	fi
}

printKerInfo() {
        echo -n " ^fg()$(uname -r)^fg(#007b8c)/^fg(#5f656b)$(uname -m) ^fg(#a488d9)| ^fg()$Uptime"
        return
}

printSpace() {
        echo -n " ^fg($COLOR_SEP)|^fg() "
        return
}

printArrow() {
        echo -n " ^fg(#a488d9)>^fg(#007b8c)>^fg(#444444)>"
        return
}

printBar() {
        while true; do
                read CPULoad0 CPULoad1 CPULoad2 CPULoad3 CPUFreq MemUsed MemPerc Uptime
                #printKerInfo
                #printSpace
                printCPUInfo
		printTempInfo
                printSpace
                printMemInfo
                #printArrow
                #echo -n "^p(+100)"
		printSpace
                printDiskInfo
                printSpace
                printVolInfo
		printSpace
		printBacklightInfo
		printSpace
		printBattery
                printSpace
                printMPDinfo
                #printSpace
                #printMail

                echo
        done
        return
}

#Print all and pipe into dzen

conky -c $CONKYFILE -u $INTERVAL | printBar | dzen2 -x $X_POS -y $Y_POS -w $WIDTH -h $HEIGHT -fn $FONT -ta 'l' -bg $DZEN_BG -fg $DZEN_FG -p -e ''
