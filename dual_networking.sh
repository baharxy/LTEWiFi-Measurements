#!/system/bin/sh
/system/xbin/pkill dhcpcd
/system/bin/svc wifi disable
/system/bin/svc data enable
/system/bin/netcfg wlan0 up
cd /data/misc/wifi/
/system/xbin/rm -rf /data/misc/wifi/sockets/wlan0
/system/bin/wpa_supplicant -B -Dnl80211 -iwlan0 -c/data/misc/wifi/wpa_supplicant.conf
/system/bin/sleep 5
/system/bin/dhcpcd wlan0
