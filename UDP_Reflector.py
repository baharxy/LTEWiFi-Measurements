import socket
import time
import sys
import logging
import pdb
logging.basicConfig(filename='time_shift.txt',level=logging.DEBUG)
import ntplib
import datetime
c = ntplib.NTPClient()

 
#DEFINE INPUTS HERE
REFLECTOR_HOST = '134.226.40.138' #IP ADDRESS TO LISTEN FOR INCOMMING PACKETS (v4 or v6)
REFLECTOR_PORT = 60000 #IP PORT TO LISTEN FOR INCOMMING PACKETS
REMOTE_PORT = 60000 #REMOTE PORT TO REFLECT PACKETS TO
REFLECT_SWITCH = 1 #REFLECTION ENABLED:1 (TWO-WAY DELAY), REFLECTION DISABLED:0 (ONE-WAY DELAY)
BUFFER = 4096


ADDR = (REFLECTOR_HOST, REFLECTOR_PORT)
 
#DUMB CHECK OF IP ADDRESS VERSION
if ':' in REFLECTOR_HOST:
 EchoServer = socket.socket(socket.AF_INET6, socket.SOCK_DGRAM)
else:
 EchoServer = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
 
#BINDING, PROCESSING AND WRITING TO CSV
try:
 EchoServer.bind(ADDR)
 print ('echo server started on port', ADDR)
except Exception:
 print '***ERROR: Port Binding Failed'

#just to get the offset from the ntp server
response = c.request('logger.scss.tcd.ie', version=3)
#time shift
txt = '%.3f' % ( response.offset)
print txt

while True:
 data, addr = EchoServer.recvfrom(BUFFER)
 #reception time
 actual_time=float(time.time()+response.offset)
 #get the content
 addlst=addr[0],addr[1]
 
 if REFLECT_SWITCH == 1:
  EchoServer.sendto('%s' % (data), addlst)
 
 splitdata = data.split(',')
 timecount = splitdata[0].strip("('")
 #measure one way delay
 #one_way_delay = (time.time() - float(timecount))
 one_way_delay = ( actual_time - float(timecount))
 packet_number = str(splitdata[1].strip("' '"))
 packet_number = packet_number.lstrip('0')


 #was there to log time shift- but may be this adds additional delay so we only do it once
 #logging.info(txt)

 
 outfile = open("udp_oneway_results_3glab_1470byte.csv", "a").write(str(time.ctime()+','+'received , '+ packet_number+' , '+str(one_way_delay)+'\n'))
 print (time.ctime()+','+'received , '+ packet_number+' , '+str(one_way_delay))
 
EchoServer.close()



