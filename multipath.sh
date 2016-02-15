#!/bin/bash
# Author: bahar
# to assign multipath routing tables for a linux machine

echo 'getting ip addresses of wifi and lte connections'
WIFI_IP_ADDR_MASK=$(sudo ip addr show wlan1 | grep "inet\b" | awk '{print $2}')
LTE_IP_ADDR_MASK=$(sudo ip addr show wwan0 | grep "inet\b" | awk '{print $2}')

WIFI_IP_ADDR=$(echo $WIFI_IP_ADDR_MASK |  awk -F'/' '{print $1}')
LTE_IP_ADDR=$(echo $LTE_IP_ADDR_MASK |  awk -F'/' '{print $1}')    


WIFI_GATEWAY=$(ipcalc $WIFI_IP_ADDR_MASK | grep HostMin  | awk -F'[ \t]+' '{print $2}')
LTE_GATEWAY=$(ipcalc $LTE_IP_ADDR_MASK | grep HostMin  | awk -F'[ \t]+' '{print $2}')

WIFI_NETWORK=$(ipcalc $WIFI_IP_ADDR_MASK | grep Network | awk -F'[ \t]+' '{print $2}')
LTE_NETWORK=$(ipcalc $LTE_IP_ADDR_MASK | grep Network | awk -F'[ \t]+' '{print $2}')

echo 'wifi gateway: ' $WIFI_GATEWAY
echo 'lte  gateway: ' $LTE_GATEWAY 

echo 'Wifi subnet: ' $WIFI_NETWORK 
echo 'lte subnet: ' $LTE_NETWORK

#flushing previous ip tables
echo 'flushing previous tables'
sudo ip route flush table 1
sudo ip route flush table 2

echo 'add multipath tables'
# This creates two different routing tables, that we use based on the source-address.
sudo ip rule add from $WIFI_IP_ADDR table 1
sudo ip rule add from $LTE_IP_ADDR  table 2

# Configure the two different routing tables
sudo ip route add $WIFI_NETWORK  dev wlan1 scope link table 1
sudo ip route add default via $WIFI_GATEWAY dev wlan1  table 1

sudo ip route add $LTE_NETWORK  dev wwan0  scope link table 2
sudo ip route add default via $LTE_GATEWAY dev wwan0 table 2

# default route for the selection process of normal internet-traffic
sudo ip route add default scope global nexthop via $WIFI_GATEWAY dev  wlan1

# show the routing tables
echo 'ip rules: '
ip rule show
echo 'ip table wifi: '
ip route show table 1
echo 'ip table lte: '
ip route show table 2
echo 'main ip table: '
ip route show

