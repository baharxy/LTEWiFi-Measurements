import socket
import time
from datetime import datetime
import sys
import select
import pdb
#from threading import Thread
import logging
logging.basicConfig(filename='time_shift.txt',level=logging.DEBUG)
import netifaces
import ntplib
import datetime
c = ntplib.NTPClient()
import threading
import subprocess
import Queue
from subprocess import Popen, PIPE
import objc
import serial
import pdb
from curses import ascii
from serial.tools import list_ports
import re
import os
import signal
from os import getpid
from sys import argv
import psutil
 
#DEFINE INPUTS HERE
#CLIENT - SENDER
UDP_DEST_IP='134.226.40.138' #IP ADDRESS TO SEND DATAGRAMS TO (v4 or v6)
UDP_DEST_PORT=60000 #IP PORT TO SEND DATAGRAMS TO
PACKET_SIZE = 1500 #DATAGRAM SIZE IN BYTES
NR_OF_PACKETS=2000 #TOTAL NR. OF PACKETS TO SEND
PACKETS_PER_SEC=100 #PACKETS PER SECON

#files and directory assignment
directory=( '/Users/bahar/Documents/tcpdumpData/python/csv/'+ datetime.datetime.now().strftime('%Y-%m-%d_%H%M%S_%f')[ :-3])
if not os.path.exists(directory):
    os.makedirs(directory)
os.chdir(directory)
file_name='udp_rtt_' + '%s' %(PACKET_SIZE) +'bytes_' +'%s' %(NR_OF_PACKETS) +'packets'+ '%s' %(PACKETS_PER_SEC)+'sec'+ datetime.datetime.now().strftime('%Y-%m-%d_%H%M%S_%f')[ :-3] +'.csv'
tcpdump_output='tcpdump_' + '%s' %(PACKET_SIZE) +'bytes_' +'%s' %(NR_OF_PACKETS) +'packets'+ '%s' %(PACKETS_PER_SEC)+'sec' + datetime.datetime.now().strftime('%Y-%m-%d_%H%M%S_%f')[ :-3] + '.csv'
channels_file='channels.csv'

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
LTE_Addr_string=(str(UDP_RECEIVE_IP_LTE) +'.' + str(UDP_RECEIVE_PORT_LTE))
wifi_Addr_string=(str(UDP_RECEIVE_IP_wifi) +'.' + str(UDP_RECEIVE_PORT_wifi))
#buffer size
BUFFER = 4096

#just to get the offset from the ntp server
response = c.request('ntp.maths.tcd.ie', version=3)
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

#function to get lte CSQ
def sniff_lte():
 ser=serial.Serial('/dev/cu.JRDDeviceInterface000010141', 115200, timeout=3)
 #check if any thing is in the buffer
 if ser.inWaiting() > 0:
    ser.flushInput()
 #send command
 command=ser.write('AT*CNTI=0\r')
 readAT(ser)
 command=ser.write('AT+CREG?\r')
 readAT(ser)
 command = ser.write('AT+CSQ\r')
 readAT(ser)

 ser.close()

def readAT(serial_port):
 #read resposnse
 print('reading at command..press enter if you are happy to continue..')
 msg=''
 while 1:
    
    if serial_port.inWaiting():
     msg = serial_port.read(serial_port.inWaiting())
     if '+CSQ' in msg:
      CSQ_line= msg.split(' ')
      CSQ_string=re.match(r'[0-9]*,[0-9]*',CSQ_line[1])
      
      CSQ=-(113-(2*float((CSQ_string.group()).replace(',','.'))))
      print ('CSQ: '+ str(CSQ)+'\n')
      open(channels_file, "a").write('RSSI:'+ str(CSQ)+'\n')
     elif  '+CREG' in msg:
      CREG_line= msg.split(',')
      LAC=int(CREG_line[2],16)
      CELL_ID= int(CREG_line[3],16)
      print('LAC:'+ str(LAC)+ ', CELL ID: '+ str(CELL_ID))
      open(channels_file, "a").write('LAC, '+ str(LAC)+ ', CELL ID, '+ str(CELL_ID)+'\n')
     elif  'CNTI' in msg:
      CNTI_line= msg.split(',')
      technology=CNTI_line[1]
      print('Technology:'+ str(technology))
      open(channels_file, "a").write('Technology, '+ str(technology)+'\n')
    if sys.stdin in select.select([sys.stdin], [], [], 0)[0]:
     line = raw_input()
     break


#function to get wifi parameters
def sniff_wifi():
 # bridge to objective c(apple stuff)
 objc.loadBundle('CoreWLAN',
                bundle_path='/System/Library/Frameworks/CoreWLAN.framework',
                module_globals=globals())

 for iname in CWInterface.interfaceNames():
    interface = CWInterface.interfaceWithName_(iname)
    wifi_parameters= 'Interface:      %s, SSID:           %s, Transmit Rate:  %s, Transmit Power: %s, RSSI:           %s' % (iname, interface.ssid(), interface.transmitRate(), interface.transmitPower(), interface.rssi())
    print wifi_parameters
    open(channels_file, "a").write(wifi_parameters+'\n')


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
 #sleep  1 secs so tcpdump starts earlier
 time.sleep(1)
 inter_departure_time = 1./PACKETS_PER_SEC
 wifi_packet_count_snd=0
 lte_packet_count_snd=0
 print 'UDP Client Started' , '\n'
 print 'UDP target IP:', UDP_DEST_IP, '\n'
 print 'UDP target port:', UDP_DEST_PORT, '\n'
 print 'UDP Packets to Send:', NR_OF_PACKETS, '\n'

 
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
     #print (datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S.%f')[:-3]+','+'sent, '+ str('%02d' % i)+ interface_type+'\n')
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
 if interface_type=='wifi':
  wifi_PLR = 100 - ((wifi_packet_count_rcvd*100.)/wifi_packet_count_snd)
  print '\n', wifi_packet_count_snd, 'wifi packets sent','\n'
  print wifi_packet_count_rcvd, 'wifi packets received','\n'
  print 'wifi packet loss ratio = ', round(wifi_PLR, 3), '%','\n'
  if wifi_packet_count_rcvd == 0:
   pass
  else:
   print 'average wifi rtt = ', wifi_cumulative_delay/wifi_packet_count_rcvd,'\n'
 elif interface_type=='lte':
  lte_PLR = 100 - ((lte_packet_count_rcvd*100.)/lte_packet_count_snd)
  print '\n', lte_packet_count_snd, 'lte packets sent','\n'
  print lte_packet_count_rcvd, 'lte packets received','\n'
  print 'lte packet loss ratio = ', round(lte_PLR, 3), '%','\n'
  if lte_packet_count_rcvd == 0:
   pass
  else:
   print 'average lte rtt = ', lte_cumulative_delay/lte_packet_count_rcvd,'\n'


#function to call tcpdump
def Readtraffic():
 print 'initialising tcpdump- lte and wifi packet counts'
 global tcpdump_lte_count
 global tcpdump_wifi_count
 tcpdump_lte_count=0
 tcpdump_wifi_count=0
 command = 'sudo tcpdump host 134.226.40.138 udp'
 dumpproc = subprocess.Popen(['sudo', 'tcpdump', '-n', 'dst' ,'host', '134.226.40.138'] , bufsize=0, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
 #dumpproc = subprocess.Popen(command,executable="/bin/bash", shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, stdin=subprocess.PIPE)
 global tcpdump_pid
 tcpdump_pid=dumpproc.pid
 print('tcpdump pid is:'+str(tcpdump_pid))
 #output = ''
 for line in iter(dumpproc.stdout.readline, ""):
 #while True:
  #line=dumpproc.stdout.readline()
  #if subprocess.Popen.poll(dumpproc) is not None:
  if len(line.split()) > 2 :
   line_split=line.split()
   if line_split[1]=='IP':
    if line_split[2]== LTE_Addr_string:
     line_split[2]='lte'
     tcpdump_lte_count=tcpdump_lte_count+1
     line_split.append(str(tcpdump_lte_count))
     new_line= " , ".join(line_split)
     outfile = open(tcpdump_output, "a").write(new_line+'\n')
     #print (new_line + '\n')
    elif line_split[2]==wifi_Addr_string:
     line_split[2]='wifi'
     tcpdump_wifi_count=tcpdump_wifi_count+1
     line_split.append(str(tcpdump_wifi_count))
     new_line= " , ".join(line_split)
     outfile = open(tcpdump_output, "a").write(new_line+'\n')
     #print (new_line+ '\n')
  elif len(line.split())< 2 or  not line:
   print 'no more data, exiting tcpdump'
   dumpproc.terminate()
   break
 print('DONE')

#def kill_processes():



#START THE THREADS FOR SENDER AND RECEIVER
if __name__ == "__main__":
    
    sniff_wifi()
    sniff_lte()
    tcpdump_thread=threading.Thread(target=Readtraffic, args=())
    #tcpdump_thread.daemon=True
    tcpdump_thread.start()
    #time.sleep(1)

    
    
    wifi_sender_thread = threading.Thread(target=udp_client_send, args=(UDP_DEST_IP, UDP_DEST_PORT, PACKET_SIZE, NR_OF_PACKETS, PACKETS_PER_SEC,response,wifi_sock,'wifi'))
    #wifi_sender_thread.daemon=True
    wifi_sender_thread.start()
    
    LTE_sender_thread = threading.Thread(target=udp_client_send, args=(UDP_DEST_IP, UDP_DEST_PORT, PACKET_SIZE, NR_OF_PACKETS, PACKETS_PER_SEC,response,LTE_sock,'lte'))
    #LTE_sender_thread.daemon=True
    LTE_sender_thread.start()
    
    wifi_receiver_thread = threading.Thread(target=udp_client_receive, args=(UDP_RECEIVE_IP_wifi, UDP_RECEIVE_PORT_wifi,response,wifi_sock,'wifi'))
    wifi_receiver_thread.daemon=True
    wifi_receiver_thread.start()

    LTE_receiver_thread = threading.Thread(target=udp_client_receive, args=(UDP_RECEIVE_IP_LTE, UDP_RECEIVE_PORT_LTE,response,LTE_sock,'lte'))
    LTE_receiver_thread.daemon=True
    LTE_receiver_thread.start()
    #time.sleep(1)
    #close all threads
    wifi_sender_thread.join()
    LTE_sender_thread.join()

    #wifi_receiver_thread.join()
    #LTE_receiver_thread.join()
    time.sleep(5)
    print 'killing tcpdump subprocess'
    os.kill(tcpdump_pid, signal.SIGKILL)
    
    time.sleep(5)
   

    sniff_wifi()
    sniff_lte()





    







