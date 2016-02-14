import os
import sys
from sys import platform as _platform
import time
import types
import pythonwifi.flags
from pythonwifi.iwlibs import Wireless, WirelessInfo, Iwrange, getNICnames, getWNICnames
     
if _platform== 'linux' or _platform== 'linux2':
 interface=Wireless('wlan1')
 stat, qual, discard, missed_beacon = interface.getStatistics()
 # Link Quality, Signal Level and Noise Level line
 wifi_parameters= 'Interface:      %s, SSID:           %s, Transmit Rate:  %s, Transmit Power: %s, RSSI:           %s'   % ('wlan1', interface.getEssid(), interface.getBitrate(), interface.getTXPower(), qual.signallevel)
 print wifi_parameters
elif _platform=='darwin':
 import objc
 objc.loadBundle('CoreWLAN',
                bundle_path='/System/Library/Frameworks/CoreWLAN.framework',
                module_globals=globals())

 for iname in CWInterface.interfaceNames():
  interface = CWInterface.interfaceWithName_(iname)
  print """
 Interface:      %s
 SSID:           %s
 Transmit Rate:  %s
 Transmit Power: %s
 RSSI:           %s""" % (iname, interface.ssid(), interface.transmitRate(),
                         interface.transmitPower(), interface.rssi())

