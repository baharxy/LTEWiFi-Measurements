import socket
import time
import sys
import pdb
from threading import Thread
import logging
logging.basicConfig(filename='time_shift.txt',level=logging.DEBUG)
import ntplib
import datetime
c = ntplib.NTPClient()
 
#DEFINE INPUTS HERE
#CLIENT - SENDER
UDP_DEST_IP='134.226.40.138' #IP ADDRESS TO SEND DATAGRAMS TO (v4 or v6)
UDP_DEST_PORT=60000 #IP PORT TO SEND DATAGRAMS TO
PACKET_SIZE = 500 #DATAGRAM SIZE IN BYTES
NR_OF_PACKETS=100 #TOTAL NR. OF PACKETS TO SEND
PACKETS_PER_SEC=100 #PACKETS PER SECOND
 
#CLIENT - RECEIVER
UDP_RECEIVE_IP = '134.226.62.105' #'127.0.0.1' IP ADDRESS TO LISTEN FOR INCOMMING PACKETS (v4 or v6)
UDP_RECEIVE_PORT=60000 #IP PORT TO LISTEN FOR INCOMMING PACKETS
BUFFER = 4096

#just to get the offset from the ntp server
response = c.request('ns1.tcd.ie', version=3)
#time shift
txt = '%.3f' % ( response.offset)
print txt
#CLIENT-RECEIVER PART
def udp_client_receive(UDP_RECEIVE_IP, UDP_RECEIVE_PORT,response):
 ADDR = (UDP_RECEIVE_IP, UDP_RECEIVE_PORT)
 
 #MAKE A DUMB IP VERSION CHECK
 if ':' in UDP_RECEIVE_IP:
  rcv_sock = socket.socket( socket.AF_INET6, socket.SOCK_DGRAM )
 else:
  rcv_sock = socket.socket( socket.AF_INET, socket.SOCK_DGRAM )
 
 global packet_count_rcvd
 global cumulative_delay
 packet_count_rcvd = 0
 cumulative_delay=0.
    
 #commented by bahar
 try:
  rcv_sock.bind(ADDR)
  print ("Server Listening on", ADDR )
 except Exception:
  print '***ERROR: Server Port Binding Failed'
 #bahar
 #FIRE UP THE LISTENER ENGINES
 while True:
  data, addr = rcv_sock.recvfrom( BUFFER )
  actual_time=float(time.time()+response.offset)
  splitdata = data.split(',')
  timecount = splitdata[0].strip("('")
  timecountnooffset=float(float(timecount)-response.offset)
  #ntp offset considered
  rt_delay = (actual_time - float(timecount))
  #ntp offset not considered
  #rt_delay = (time.time() - float(timecount))
  packet_number = str(splitdata[1].strip("' '"))
  packet_number = packet_number.lstrip('0')
  #WRITE TO FILE AND DO PACKET COUNT
  outfile = open("udp_twoway_results_3glab_1500bytes.csv", "a").write(str(time.ctime()+','+'received , '+ packet_number+' , '+str(rt_delay)+'\n'))
  print (time.ctime()+','+'received , '+ packet_number+' , '+ str('%.5f' % timecountnooffset) +'sent time, '+  str('%.5f' % float (actual_time-response.offset)) + 'received time, ' +str(rt_delay)+'delay')
  packet_count_rcvd=packet_count_rcvd+1
  cumulative_delay=cumulative_delay+rt_delay
  rcv_sock.close()
#CLIENT SERVER SIDE
def udp_client_send(UDP_DEST_IP, UDP_DEST_PORT, PACKET_SIZE, NR_OF_PACKETS, PACKETS_PER_SEC,response):
 
 inter_departure_time = 1./PACKETS_PER_SEC
 packet_count_snd=0
 
 print "UDP Client Started"
 print "UDP target IP:", UDP_DEST_IP
 print "UDP target port:", UDP_DEST_PORT
 print "UDP Packets to Send:", NR_OF_PACKETS

 
 #IF IPv6
 if ':' in UDP_DEST_IP:
  if PACKET_SIZE > 97:
   #BUILD DATAGRAM OF DESIRED SIZE
   padding=''
   for j in range (98, PACKET_SIZE):
     padding = padding+str(1)
   for i in range (1,NR_OF_PACKETS+1):
     #SEND SPECIFIED NUMBER OF PACKETS
     time.sleep(inter_departure_time )
     snd_sock6 = socket.socket( socket.AF_INET6,socket.SOCK_DGRAM )
     snd_sock6.sendto(str(("%.5f" % time.time(),str('%08d' % i), padding)), (UDP_DEST_IP, UDP_DEST_PORT) )
     packet_count_snd = packet_count_snd+1
 
 #IF NOT IPv6
 else:
  if PACKET_SIZE >  77:
   padding=''
   for j in range (78, PACKET_SIZE):
     padding = padding+str(1)
   for i in range (1,NR_OF_PACKETS+1):
     time.sleep(inter_departure_time)
     snd_sock = socket.socket( socket.AF_INET,socket.SOCK_DGRAM )
     #Try to bind to send socket??
     try:
      snd_sock.bind(('134.226.62.105',65000))
      #print ("Clie", ADDR )
     except Exception:
      print '***ERROR: Client Port Binding Failed'
     #timestamp+ntp offset
     snd_sock.sendto(str(("%.5f" % float(time.time()+response.offset),str('%08d' % i), padding)), (UDP_DEST_IP, UDP_DEST_PORT) )
     #ntp offset is not considered
     #snd_sock.sendto(str(("%.5f" % float(time.time()),str('%08d' % i), padding)), (UDP_DEST_IP, UDP_DEST_PORT) )
     #was there to log time shift- but may be this adds additional delay so we only do it once
     #logging.info(txt)
     packet_count_snd = packet_count_snd+1
     snd_sock.close()
     
 #WAIT 5SEC FOR ALL PACKETS TO ARRIVE
 time.sleep(5)
 PLR = 100 - ((packet_count_rcvd*100.)/packet_count_snd)
 
 print '\n', packet_count_snd, 'packets sent'
 print packet_count_rcvd, 'packets received'
 print 'packet loss ratio = ', round(PLR, 3), '%'
 
 if packet_count_rcvd == 0:
  pass
 else:
  print 'average rtt = ', cumulative_delay/packet_count_rcvd
 
#START THE THREADS FOR SENDER AND RECEIVER
if __name__ == "__main__":
    receiver_thread = Thread(target=udp_client_receive, args=(UDP_RECEIVE_IP, UDP_RECEIVE_PORT,response))
    receiver_thread.daemon=True
    receiver_thread.start()
    time.sleep(1)
    sender_thread = Thread(target=udp_client_send, args=(UDP_DEST_IP, UDP_DEST_PORT, PACKET_SIZE, NR_OF_PACKETS, PACKETS_PER_SEC,response)).start()
#udp_client_send(UDP_DEST_IP, UDP_DEST_PORT, PACKET_SIZE, NR_OF_PACKETS, PACKETS_PER_SEC,response)