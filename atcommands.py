import serial
import pdb
from curses import ascii
from serial.tools import list_ports
import time
import os 
from sys import platform as _platform 

if _platform == "linux" or _platform == "linux2":
  plat = 'LINUX'
  print "The operating system is Linux"
elif _platform == "darwin":
  plat = 'OS_X'
  import objc
  print  "The operating system is os x"

#pdb.set_trace()
if plat=='OS_X':
 ser=serial.Serial('/dev/cu.JRDDeviceInterface000010141', 115200, timeout=3)
elif plat=='LINUX':
 ser=serial.Serial('/dev/ttyUSB2', 115200, timeout=3)

#check if any thing is in the buffer
if ser.inWaiting() > 0:
    ser.flushInput()
#send command
command= ser.write("AT*CNTI=0\r")
if plat=='LINUX':
 response=ser.read(1024)
 print response
 ser.close()
#time.sleep(5)
#command=ser.write('AT+CREG?\r')
#command = ser.write('AT!GSTATUS?\r')
#print command
#pdb.set_trace()
#read resposnse
elif plat=='OS_X':
 while 1:
   if ser.inWaiting():
      msg = ser.read(ser.inWaiting())
      print msg
 print msg
 ser.close()

