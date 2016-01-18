import socket
import time
import sys
import pdb
from threading import Thread
import logging
logging.basicConfig(filename='time_shift.txt',level=logging.DEBUG)
import netifaces
import ntplib
import datetime
c = ntplib.NTPClient()
 
#DEFINE INPUTS HERE
#CLIENT - SENDER
UDP_DEST_IP='134.226.40.138' #IP ADDRESS TO SEND DATAGRAMS TO (v4 or v6)
UDP_DEST_PORT=60000 #IP PORT TO SEND DATAGRAMS TO
PACKET_SIZE = 1500 #DATAGRAM SIZE IN BYTES
NR_OF_PACKETS=1000 #TOTAL NR. OF PACKETS TO SEND
PACKETS_PER_SEC=100 #PACKETS PER SECOND
file_name='udp_rtt_' + '%s' %(PACKET_SIZE) +'bytes_' +'%s' %(NR_OF_PACKETS) +'packets'+ '%s' %(PACKETS_PER_SEC)+'sec_lab.csv'

#get ip addresses of the avialable interfaces
if 'en0' not in netifaces.interfaces():
 print 'warning: no wifi connection'

if 'en4' not in netifaces.interfaces():
 print 'warning: no LTE connection'

ip_list = []
for interface in netifaces.interfaces():
 if interface=='en0' or interface=='en4':
  for link in netifaces.ifaddresses(interface)[netifaces.AF_INET]:
   ip_list.append(link['addr'])

#CLIENT - RECEIVER
#Wifi Interface
UDP_RECEIVE_IP_wifi =ip_list[0] #'127.0.0.1' IP ADDRESS TO LISTEN FOR INCOMMING PACKETS (v4 or v6)
UDP_RECEIVE_PORT_wifi=60000 #IP PORT TO LISTEN FOR INCOMMING PACKETS
#4g interface
UDP_RECEIVE_IP_LTE = ip_list[1] #'127.0.0.1' IP ADDRESS TO LISTEN FOR INCOMMING PACKETS (v4 or v6)
UDP_RECEIVE_PORT_LTE=60000 #IP PORT TO LISTEN FOR INCOMMING PACKETS
#buffer size
BUFFER = 4096

#just to get the offset from the ntp server
response = c.request('ns1.tcd.ie', version=3)
#time shift
txt = '%.3f' % ( response.offset)
print txt
#make a socket for wifi transmisions
ADDR_wifi = (UDP_RECEIVE_IP_wifi, UDP_RECEIVE_PORT_wifi)
wifi_sock= socket.socket( socket.AF_INET, socket.SOCK_DGRAM )
try:
 wifi_sock.bind(ADDR_wifi)
 print ("binding to ", ADDR_wifi )
except Exception:
 print '***ERROR: Server Port Binding Failed (wifi interface)'
#make a socket for LTE transmissions
ADDR_LTE = (UDP_RECEIVE_IP_LTE, UDP_RECEIVE_PORT_LTE)
LTE_sock= socket.socket( socket.AF_INET, socket.SOCK_DGRAM )
try:
    LTE_sock.bind(ADDR_LTE)
    print ("binding to ", ADDR_LTE )
except Exception:
    print '***ERROR: Server Port Binding Failed (lte interface)'



#CLIENT-RECEIVER PART
def udp_client_receive(UDP_RECEIVE_IP, UDP_RECEIVE_PORT,response,input_sock, interface_type):
 
 global wifi_packet_count_rcvd
 global lte_packet_count_rcvd
 global wifi_cumulative_delay
 global lte_cumulative_delay
 wifi_packet_count_rcvd = 0
 lte_packet_count_rcvd=0
 wifi_cumulative_delay=0
 lte_cumulative_delay=0
    
  #FIRE UP THE LISTENER ENGINES
 while True:
  data, addr = input_sock.recvfrom( BUFFER )
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
  outfile = open(file_name, "a").write(str(time.ctime()+','+'received , '+ packet_number+' , '+str(rt_delay)+ ','+  interface_type+'\n'))
  #print (time.ctime()+','+'received , '+ packet_number+' , '+ str('%.5f' % timecountnooffset) +'sent time, '+  str('%.5f' % float (actual_time-response.offset)) + 'received time, ' +str(rt_delay)+'delay')
  if interface_type=='wifi':
   wifi_packet_count_rcvd=wifi_packet_count_rcvd+1
   wifi_cumulative_delay=wifi_cumulative_delay+rt_delay
  elif interface_type=='lte':
   lte_packet_count_rcvd=lte_packet_count_rcvd+1
   lte_cumulative_delay=lte_cumulative_delay+rt_delay


  
#CLIENT SERVER SIDE
def udp_client_send(UDP_DEST_IP, UDP_DEST_PORT, PACKET_SIZE, NR_OF_PACKETS, PACKETS_PER_SEC,response, input_sock,interface_type):
 
 inter_departure_time = 1./PACKETS_PER_SEC
 wifi_packet_count_snd=0
 lte_packet_count_snd=0
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
     #snd_sock = socket.socket( socket.AF_INET,socket.SOCK_DGRAM )
     #Try to bind to send socket??
     #try:
      #snd_sock.bind(('0.0.0.0',65000))
      #print ("Clie", ADDR )
     #except Exception:
      #print '***ERROR: Client Port Binding Failed'
     #timestamp+ntp offset
     input_sock.sendto(str(("%.5f" % float(time.time()+response.offset),str('%08d' % i), interface_type, padding)), (UDP_DEST_IP, UDP_DEST_PORT) )
     #print (time.ctime()+','+'sent, '+ str('%08d' % i))
     #ntp offset is not considered
     #snd_sock.sendto(str(("%.5f" % float(time.time()),str('%08d' % i), padding)), (UDP_DEST_IP, UDP_DEST_PORT) )
     #was there to log time shift- but may be this adds additional delay so we only do it once
     #logging.info(txt)
     if interface_type=='wifi':
      wifi_packet_count_snd = wifi_packet_count_snd+1
     elif interface_type=='lte':
      lte_packet_count_snd = lte_packet_count_snd+1
 
 #WAIT 5SEC FOR ALL PACKETS TO ARRIVE
 time.sleep(5)
 if interface_type=='wifi':
  wifi_PLR = 100 - ((wifi_packet_count_rcvd*100.)/wifi_packet_count_snd)
  print '\n', wifi_packet_count_snd, 'wifi packets sent'
  print wifi_packet_count_rcvd, 'wifi packets received'
  print 'wifi packet loss ratio = ', round(wifi_PLR, 3), '%'
  if wifi_packet_count_rcvd == 0:
   pass
  else:
   print 'average wifi rtt = ', wifi_cumulative_delay/wifi_packet_count_rcvd
 elif interface_type=='lte':
  lte_PLR = 100 - ((lte_packet_count_rcvd*100.)/lte_packet_count_snd)
  print '\n', lte_packet_count_snd, 'lte packets sent'
  print lte_packet_count_rcvd, 'lte packets received'
  print 'lte packet loss ratio = ', round(lte_PLR, 3), '%'
  if lte_packet_count_rcvd == 0:
   pass
  else:
   print 'average lte rtt = ', lte_cumulative_delay/lte_packet_count_rcvd


 
#START THE THREADS FOR SENDER AND RECEIVER
if __name__ == "__main__":
    wifi_sender_thread = Thread(target=udp_client_send, args=(UDP_DEST_IP, UDP_DEST_PORT, PACKET_SIZE, NR_OF_PACKETS, PACKETS_PER_SEC,response,wifi_sock,'wifi'))
    wifi_sender_thread.start()
    
    LTE_sender_thread = Thread(target=udp_client_send, args=(UDP_DEST_IP, UDP_DEST_PORT, PACKET_SIZE, NR_OF_PACKETS, PACKETS_PER_SEC,response,LTE_sock,'lte'))
    LTE_sender_thread.start()
    
    wifi_receiver_thread = Thread(target=udp_client_receive, args=(UDP_RECEIVE_IP_wifi, UDP_RECEIVE_PORT_wifi,response,wifi_sock,'wifi'))
    wifi_receiver_thread.daemon=True
    wifi_receiver_thread.start()

    LTE_receiver_thread = Thread(target=udp_client_receive, args=(UDP_RECEIVE_IP_LTE, UDP_RECEIVE_PORT_LTE,response,LTE_sock,'lte'))
    LTE_receiver_thread.daemon=True
    LTE_receiver_thread.start()
    time.sleep(1)
#udp_client_send(UDP_DEST_IP, UDP_DEST_PORT, PACKET_SIZE, NR_OF_PACKETS, PACKETS_PER_SEC,response)