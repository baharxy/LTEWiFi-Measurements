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

