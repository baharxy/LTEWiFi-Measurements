#!/system/bin/sh
/system/xbin/pkill dhcpcd
/system/xbin/pkill wpa_supplicant
/system/xbin/rm -rf /data/misc/wifi/sockets/wlan0
/system/bin/netcfg wlan0 down
/system/bin/svc wifi disable
/system/bin/svc data disable
/system/xbin/echo 0 >/proc/sys/net/ipv4/ip_forward
