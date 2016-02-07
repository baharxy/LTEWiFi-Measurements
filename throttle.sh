#!/bin/bash

# Uses  /Users/bahar/Documents/tcpdumpData/pftable to hold a list of IPs or subnets to throttle.
# Example file contents:
# 1.2.3.4
# 2.3.4.5/16

# Reset dummynet to default config
dnctl -f flush

# Compose an addendum to the default config to create a new anchor and table file
read -d '' -r PF <<EOF
dummynet-anchor "myanchor"
anchor "myanchor"
table <pftable> persist file "/Users/bahar/Documents/tcpdumpData/pftable"
EOF

# Reset PF to default config and apply our addendum
(cat /etc/pf.conf && echo "$PF") | pfctl -q -f -

# Configure the new anchor
cat <<EOF | pfctl -q -a myanchor -f -
no dummynet quick on lo0 all
dummynet out proto udp  from any to <pftable> port 60000 pipe 1
EOF

# Create the dummynet queue
dnctl pipe 1 config  delay 300ms

# Show new configs
printf "\nGlobal pf dummynet anchors:\n"
pfctl -q -s dummynet
printf "\nmyanchor config:\n"
pfctl -q -s dummynet -a myanchor
printf "\npftable config:\n"
pfctl -q -t pftable -T show
printf "\ndummynet config:\n"
dnctl show queue

# Enable PF
sudo pfctl -E
