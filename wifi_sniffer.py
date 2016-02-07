#!/usr/bin/env python
import scapy
from scapy.all import *
import pdb

ap_list = []

def PacketHandler(pkt) :
  pdb.set_trace()
  if pkt.haslayer(Dot11) :
                pdb.set_trace()
		if pkt.type == 0 and pkt.subtype == 8 :
			if pkt.addr2 not in ap_list :
				ap_list.append(pkt.addr2)
				print "AP MAC: %s with SSID: %s " %(pkt.addr2, pkt.info)


sniff(iface="en0", prn = PacketHandler)
