import serial
import pdb
from curses import ascii
from serial.tools import list_ports
import time

#pdb.set_trace()
ser=serial.Serial('/dev/cu.JRDDeviceInterface000010141', 115200, timeout=3)
#check if any thing is in the buffer
if ser.inWaiting() > 0:
    ser.flushInput()
#send command
command= ser.write('AT*CNTI=0\r')
#time.sleep(5)
#command=ser.write('AT+CREG?\r')
#command = ser.write('AT!GSTATUS?\r')
#print command
#pdb.set_trace()
#read resposnse

while 1:
   if ser.inWaiting():
      msg = ser.read(ser.inWaiting())
      print msg
print msg
ser.close()

