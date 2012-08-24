#!/bin/bash

KB_Layout=`skb -1`
if [ $KB_Layout == "Rus" ]; then
	xdotool key ISO_Next_Group
fi
