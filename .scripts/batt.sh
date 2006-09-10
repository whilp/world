#!/usr/bin/env sh

GREP=/usr/bin/grep

BATT=/proc/acpi/battery/BAT0
BATT_INFO=$BATT/info
BATT_STATE=$BATT/state

$GREP -i 'last full capacity' $
