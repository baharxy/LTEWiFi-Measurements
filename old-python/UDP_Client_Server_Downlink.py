import socket
import time
from datetime import datetime
import sys
import select
import pdb
#from threading import Thread
import logging
logging.basicConfig(filename='time_shift.txt',level=logging.DEBUG)
import datetime
import threading
import subprocess
import Queue
from subprocess import Popen, PIPE
import pdb
from curses import ascii
import re
import os
import signal
from os import getpid
from sys import argv


#DEFINE INPUTS HERE
#CLIENT - SENDER
SERVER_HOST = '134.226.40.138' #IP ADDRESS TO LISTEN FOR INCOMMING PACKETS (v4 or v6)
SERVER_PORT = 60000 #IP PORT TO LISTEN FOR INCOMMING PACKETS
BUFFER = 4096
PACKET_SIZE = 1500 #DATAGRAM SIZE IN BYTES
NR_OF_PACKETS=2000 #TOTAL NR. OF PACKETS TO SEND
PACKETS_PER_SEC=100 #PACKETS PER SECOND

#files and directory assignment
directory=( '/home/partovb/bahar/python/csv/'+ datetime.datetime.now().strftime('%Y-%m-%d_%H%M%S_%f')[ :-3])
if not os.path.exists(directory):
    os.makedirs(directory)
os.chdir(directory)
file_name='udp_rtt_' + '%s' %(PACKET_SIZE) +'bytes_' +'%s' %(NR_OF_PACKETS) +'packets'+ '%s' %(PACKETS_PER_SEC)+'sec'+ datetime.datetime.now().strftime('%Y-%m-%d_%H%M%S_%f')[ :-3] +'.csv'
tcpdump_output='tcpdump_' + '%s' %(PACKET_SIZE) +'bytes_' +'%s' %(NR_OF_PACKETS) +'packets'+ '%s' %(PACKETS_PER_SEC)+'sec' + datetime.datetime.now().strftime('%Y-%m-%d_%H%M%S_%f')[ :-3] + '.csv'




#just to get the offset from the ntp server
command = "ntpq -nc peers | tail -n +3 | grep \* | cut -c 62-71| tr -d ' '   "
ntproc = os.popen(command)
offset=float(  ntproc.read()) / 1000
#offset = c.request('ntp.maths.tcd.ie', version=3)
#time shift
txt = ('ntp offset is: %.3f'  %(offset) +'\n')
print txt


ADDR = (SERVER_HOST, SERVER_PORT)

#DUMB CHECK OF IP ADDRESS VERSION
if ':' in SERVER_HOST:
    ServerSocket = socket.socket(socket.AF_INET6, socket.SOCK_DGRAM)
else:
    ServerSocket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)

#BINDING, PROCESSING AND WRITING TO CSV
try:
    ServerSocket.bind(ADDR)
    print ('lily server started on port', ADDR)
except Exception:
    print '***ERROR: Port Binding Failed'

#Receive initialisation pattern
NR_OF_RECEIVED_PACKETS=0
if (NR_OF_RECEIVED_PACKETS < 3):
 print 'receiving hand shake messages from the local machine'
 while True:
  data, addr = ServerSocket.recvfrom( BUFFER )
  actual_time=float(time.time()+offset)
  splitdata = data.split(',')
  timecount = splitdata[0].strip("('")
  #ntp offset considered
  rt_delay = (actual_time - float(timecount))
  #ntp offset not considered
  #rt_delay = (time.time() - float(timecount))
  packet_number = str(splitdata[1].strip("' '"))
  packet_number = packet_number.lstrip('0')
  client_interface=str(splitdata[2].strip("' '"))
  print client_interface
  if client_interface=='lte':
   NAT_IP=addr[0]
   NAT_PORT=addr[1]
   NAT_ADDR=(NAT_IP,NAT_PORT)
   print 'NAT IP/Port: %s / %s' %(NAT_IP , NAT_PORT)
  elif client_interface=='wifi':
   wifi_IP=addr[0]
   wifi_PORT=addr[1]
   wifi_ADDR=(wifi_IP,wifi_PORT)
   print 'WiFi IP/Port: %s / %s' %(wifi_IP , wifi_PORT)
  print (time.ctime()+','+'received , '+ packet_number)
  NR_OF_RECEIVED_PACKETS= NR_OF_RECEIVED_PACKETS+1
  if NR_OF_RECEIVED_PACKETS == 2:
   success=1
   break

  
#CLIENT SERVER SIDE
def udp_server_send(IP, PORT, PACKET_SIZE, NR_OF_PACKETS, PACKETS_PER_SEC,offset, input_sock,interface_type):
 #sleep  1 secs so tcpdump starts earlier
 time.sleep(1)
 inter_departure_time = 1./PACKETS_PER_SEC
 wifi_packet_count_snd=0
 lte_packet_count_snd=0
 print 'UDP Sever Started' , '\n'
 print 'UDP Packets to Send:', NR_OF_PACKETS, '\n'

 
 #IF IPv6
 if ':' in NAT_IP:
  if PACKET_SIZE > 97:
   #BUILD DATAGRAM OF DESIRED SIZE
   padding=''
   for j in range (98, PACKET_SIZE):
     padding = padding+str(1)
   for i in range (1,NR_OF_PACKETS+1):
     #SEND SPECIFIED NUMBER OF PACKETS
     time.sleep(inter_departure_time )
     snd_sock6 = socket.socket( socket.AF_INET6,socket.SOCK_DGRAM )
     snd_sock6.sendto(str(("%.5f" % time.time(),str('%08d' % i), padding)), (NAT_IP, NAT_PORT) )
     packet_count_snd = packet_count_snd+1
 
 #IF NOT IPv6
 else:
  if PACKET_SIZE >  77:
   padding=''
   for j in range (78, PACKET_SIZE):
     padding = padding+str(1)
   for i in range (1,NR_OF_PACKETS+1):
     time.sleep(inter_departure_time)
     input_sock.sendto(str(("%.5f" % float(time.time()+ offset),str('%08d' % i), interface_type, padding)), (IP, PORT) )
     print (datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S.%f')[:-3]+','+'sent, '+ str('%02d' % i)+ interface_type+'\n')
     #print (datetime.datetime.now()  +  ',' + 'sent, ' + str('%08d' %i) )
     #ntp offset is not considered
     #input_sock.sendto(str(("%.5f" % float(time.time()),str('%08d' % i), padding)), (UDP_DEST_IP, UDP_DEST_PORT) )
     #was there to log time shift- but may be this adds additional delay so we only do it once
     #logging.info(txt)
     if interface_type=='wifi':
      wifi_packet_count_snd = wifi_packet_count_snd+1
     elif interface_type=='lte':
      lte_packet_count_snd = lte_packet_count_snd+1
 
 #WAIT 5SEC FOR ALL PACKETS TO ARRIVE
 time.sleep(5)
 

#function to call tcpdump



#START THE THREADS FOR SENDER AND RECEIVER
if __name__ == "__main__":

   
    #tcpdump_thread=threading.Thread(target=Readtraffic, args=())
    #tcpdump_thread.daemon=True
    #tcpdump_thread.start()
    #time.sleep(1)
  
 if success==1:
   
    
    wifi_sender_thread = threading.Thread(target=udp_server_send, args=(wifi_IP, wifi_PORT, PACKET_SIZE, NR_OF_PACKETS, PACKETS_PER_SEC,offset,ServerSocket,'wifi'))
    #wifi_sender_thread.daemon=True
    wifi_sender_thread.start()
    
    LTE_sender_thread = threading.Thread(target=udp_server_send, args=(NAT_IP, NAT_PORT, PACKET_SIZE, NR_OF_PACKETS, PACKETS_PER_SEC,offset,ServerSocket,'lte'))
    #LTE_sender_thread.daemon=True
    LTE_sender_thread.start()
    
   
    #time.sleep(1)
    #close all threads
    wifi_sender_thread.join()
    LTE_sender_thread.join()

    #wifi_receiver_thread.join()
    #LTE_receiver_thread.join()
    #time.sleep(5)
    #print 'killing tcpdump subprocess'
    #os.kill(tcpdump_pid, signal.SIGKILL)
    
    time.sleep(5)
   

    #sniff_wifi()
    #sniff_lte()
 elif success != 1:
   print "hand shake unsuccessful, try again"





    







