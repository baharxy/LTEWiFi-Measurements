__author__ = 'bahar'
import socket
import time
from datetime import datetime
import sys, getopt
from sys import platform as _platform
from sys import argv
import select
import pdb
import logging
import netifaces
import datetime
import threading
import subprocess
from subprocess import Popen, PIPE
import serial
import pdb
from curses import ascii
from serial.tools import list_ports
import re
import os
from os import getpid
import signal
import psutil
import types
import pythonwifi.flags
from pythonwifi.iwlibs import Wireless, WirelessInfo, Iwrange, getNICnames, getWNICnames
 

#get the opetating system
if _platform == "linux" or _platform == "linux2":
  plat = 'LINUX'
  print "The operating system is Linux"
elif _platform == "darwin":
  plat = 'OSX'
  import objc
  print  "The operating system is os x"
#get input options
try:
    options, remainder = getopt.getopt(sys.argv[1:], 'a:p:m:s:h', ['SERVER_IP','SERVER_PORT=',
                                                                 'MODE=',
                                                                 'PACKET_SIZE=',
                                                                 'help'                                                             ])
except getopt.GetoptError as e:
    print (str(e))
    print("Usage: %s -a server ip -p server port -m u/d mode -s packet_size in bytes" % sys.argv[0])
    sys.exit(2)
for opt, arg in options:
    if opt in ('-a', '--SERVER_IP'):
        SERVER_IP = arg
    if opt in ('-p', '--SERVER_PORT'):
        SERVER_PORT = int(arg)
    elif opt in ('-m', '--MODE'):
        MODE = arg
    elif opt in ('-s', '--PACKET_SIZE'):
        PACKET_SIZE =int(arg)
    elif opt == '--help':
        print("Usage: %s -a server ip -p server port -m u/d mode -s packet_size in bytes" % sys.argv[0])
print 'SERVER_IP   :', SERVER_IP
print 'SERVER_PORT   :', SERVER_PORT
print 'MODE   :', MODE
print 'PACKET SIZE    :', PACKET_SIZE






#DEFINE INPUTS HERE
#CLIENT - SENDER
NR_OF_PACKETS=5000 #TOTAL NR. OF PACKETS TO SEND
PACKETS_PER_SEC=100 #PACKETS PER SECON
RATE= float(float(PACKET_SIZE) * float(PACKETS_PER_SEC) *  8 / 1000000 )#data rate mbps
print ('Transmission rate is:  %3f'  %(RATE) + ' Mbit/s')

#files and directory assignment

directory=( os.getcwd()+'/csv/'+ datetime.datetime.now().strftime('%Y-%m-%d_%H%M%S_%f')[ :-3]+'_'+plat+'_'+MODE)
channels_file='channels.csv'
if not os.path.exists(directory):
    os.makedirs(directory)
    os.chdir(directory)
if MODE=='uplink' :
 file_name='udp_rtt_' + '%s' %(PACKET_SIZE) +'bytes_' +'%s' %(NR_OF_PACKETS) +'packets'+ '%s' %(PACKETS_PER_SEC)+'sec'+ datetime.datetime.now().strftime('%Y-%m-%d_%H%M%S_%f')[ :-3] +'.csv'
 tcpdump_output='tcpdump_' + '%s' %(PACKET_SIZE) +'bytes_' +'%s' %(NR_OF_PACKETS) +'packets'+ '%s' %(PACKETS_PER_SEC)+'sec' + datetime.datetime.now().strftime('%Y-%m-%d_%H%M%S_%f')[ :-3] + '.csv'
elif MODE=='downlink':
 file_name='udp_oneway_' + '%s' %(PACKET_SIZE) +'bytes_' +'%s' %(NR_OF_PACKETS) +'packets'+ '%s' %(PACKETS_PER_SEC)+'sec'+ datetime.datetime.now().strftime('%Y-%m-%d_%H%M%S_%f')[ :-3] +'.csv'

#write transmission rate to the channel file
open(channels_file, "a").write('Transmission rate: '+ str(RATE)+'  Mbit/s' +'\n')

#get ip addresses of the avialable interfaces
#if 'en0' not in netifaces.interfaces():
# print 'warning: no wifi connection'

#if 'en4' not in netifaces.interfaces():
# print 'warning: no LTE connection'
if plat == 'OSX':
 ip_list = []
 for   interface  in netifaces.interfaces():
  if interface=='en0' or interface=='en4':
   for link in netifaces.ifaddresses(interface)[netifaces.AF_INET]:
    ip_list.append(link['addr'])
elif plat == 'LINUX':
 ip_list = []
 for   interface  in netifaces.interfaces():
  if interface=='wlan1' or interface=='wwan0':
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
command = "ntpq -nc peers | tail -n +3 | grep \* | cut -c 62-71| tr -d ' '   "
ntproc = os.popen(command)
offset=float(  ntproc.read()) / 1000
#offset = c.request('ntp.maths.tcd.ie', version=3)
#time shift
txt = ('ntp offset is: %.3f'  %(offset) +'\n')
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
 if plat=='OSX':
  ser=serial.Serial('/dev/cu.JRDDeviceInterface000010141', 115200, timeout=3)
 elif plat=='LINUX':
  ser=serial.Serial('/dev/ttyUSB2', 115200, timeout=3)
 #check if any thing is in the buffer
 if ser.inWaiting() > 0:
    ser.flushInput()
 #send command
 command=ser.write('AT*CNTI=0\r')
 readAT(ser)
 #command=ser.write('AT+CREG?\r')
 #readAT(ser)
 #command = ser.write('AT+CSQ\r')
 #readAT(ser)

 ser.close()

def readAT(serial_port):
 #read resposnse
 print('reading at command..press enter if you are happy to continue..')
 if plat=='OSX':
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
 elif plat=='LINUX':
  msg=''
  msg=serial_port.read(1024)
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
 


#function to get wifi parameters --- OS X
def sniff_wifi():
 if plat=='OSX':
  # bridge to objective c(apple stuff)
  objc.loadBundle('CoreWLAN',
                bundle_path='/System/Library/Frameworks/CoreWLAN.framework',
                module_globals=globals())

  for iname in CWInterface.interfaceNames():
    interface = CWInterface.interfaceWithName_(iname)
    wifi_parameters= 'Interface:      %s, SSID:           %s, Transmit Rate:  %s, Transmit Power: %s, RSSI:           %s' % (iname, interface.ssid(), interface.transmitRate(), interface.transmitPower(), interface.rssi())
 elif plat=='LINUX':
  interface=Wireless('wlan1')
  stat, qual, discard, missed_beacon = interface.getStatistics()
  # Link Quality, Signal Level and Noise Level line
  wifi_parameters= 'Interface:      %s, SSID:           %s, Transmit Rate:  %s, Transmit Power: %s, RSSI:           %s'   % ('wlan1',   interface.getEssid(), interface.getBitrate(), interface.getTXPower(), qual.signallevel)
 
 #record wifi parameters
 print wifi_parameters
 open(channels_file, "a").write(wifi_parameters+'\n')
 


#Local-RECEIVE
def udp_client_receive(IP, PORT,offset,input_sock, interface_type):
 
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
  actual_time=float(time.time()+offset)
  splitdata = data.split(',')
  timecount = splitdata[0].strip("('")
  timecountnooffset=float(float(timecount)-offset)
  #ntp offset considered
  rt_delay = (actual_time - float(timecount))
  packet_number = str(splitdata[1].strip("' '"))
  packet_number = packet_number.lstrip('0')
  client_interface=str(splitdata[2].strip("' '"))
  #WRITE TO FILE AND DO PACKET COUNT
  outfile = open(file_name, "a").write(str(str(actual_time)+','+'received , '+ packet_number+' , '+str(rt_delay)+', '+client_interface+'\n'))
  print (time.ctime()+','+'received , '+ packet_number+' , '+str(rt_delay)+', '+client_interface)
  if client_interface=='wifi':
   wifi_packet_count_rcvd=wifi_packet_count_rcvd+1
   wifi_cumulative_delay=wifi_cumulative_delay+rt_delay
  elif client_interface=='lte':
   lte_packet_count_rcvd=lte_packet_count_rcvd+1
   lte_cumulative_delay=lte_cumulative_delay+rt_delay

 time.sleep(10)
  
#Local-Send
def udp_client_send(IP, PORT, PACKET_SIZE, NR_OF_PACKETS, PACKETS_PER_SEC,offset, input_sock,interface_type):
 #sleep  1 secs so tcpdump starts earlier
 time.sleep(1)
 inter_departure_time = 1./PACKETS_PER_SEC
 wifi_packet_count_snd=0
 lte_packet_count_snd=0
 print 'UDP Client Started' , '\n'
 print 'UDP target IP:', SERVER_IP, '\n'
 print 'UDP target port:', SERVER_PORT, '\n'
 print 'UDP Packets to Send:', NR_OF_PACKETS, '\n'

 
 #IF IPv6
 if ':' in SERVER_IP:
  if PACKET_SIZE > 97:
   #BUILD DATAGRAM OF DESIRED SIZE
   padding=''
   for j in range (98, PACKET_SIZE):
     padding = padding+str(1)
   for i in range (1,NR_OF_PACKETS+1):
     #SEND SPECIFIED NUMBER OF PACKETS
     time.sleep(inter_departure_time )
     snd_sock6 = socket.socket( socket.AF_INET6,socket.SOCK_DGRAM )
     snd_sock6.sendto(str(("%.5f" % time.time(),str('%08d' % i), padding)), (IP, PORT) )
     packet_count_snd = packet_count_snd+1
 
 #IF NOT IPv6
 else:
  if PACKET_SIZE >  50:
   padding=''
   for j in range (51, PACKET_SIZE):
     padding = padding+str(1)
   for i in range (1,NR_OF_PACKETS+1):
     time.sleep(inter_departure_time)
     if NR_OF_PACKETS==1:
      input_sock.sendto(  str("%.5f" % float(time.time()+ offset))+','+ str('%08d' % i)+ ','+interface_type+','+plat , (IP, PORT) )
     else:
      input_sock.sendto(str(("%.5f" % float(time.time()+ offset),str('%08d' % i), interface_type, padding)), (IP, PORT) )
     if interface_type=='wifi':
      wifi_packet_count_snd = wifi_packet_count_snd+1
     elif interface_type=='lte':
      lte_packet_count_snd = lte_packet_count_snd+1
 
 #WAIT 5SEC FOR ALL PACKETS TO ARRIVE
 time.sleep(5)


#function to call tcpdump
def Readtraffic():
 print 'initialising tcpdump- lte and wifi packet counts'
 global tcpdump_lte_count
 global tcpdump_wifi_count
 tcpdump_lte_count=0
 tcpdump_wifi_count=0
 command = 'sudo tcpdump host 134.226.40.138 udp'
 if plat=='OSX':
  dumpproc = subprocess.Popen(['sudo', 'tcpdump', '-n', 'dst' ,'host', '134.226.40.138'] , bufsize=0, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
 elif plat=='LINUX':
  dumpproc = subprocess.Popen(['sudo', 'tcpdump', '-i','any','-n', 'dst' ,'host', '134.226.40.138'] , bufsize=0, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
 global tcpdump_pid
 tcpdump_pid=dumpproc.pid
 print('tcpdump pid is:'+str(tcpdump_pid))
 #output = ''
 for line in iter(dumpproc.stdout.readline, ""):
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




#START THE THREADS FOR SENDER AND RECEIVER
if __name__ == "__main__":
    
 if MODE=='uplink':
    sniff_wifi()
    sniff_lte()
    tcpdump_thread=threading.Thread(target=Readtraffic, args=())
    #tcpdump_thread.daemon=True
    tcpdump_thread.start()
    #time.sleep(1)
    #initiate  send/recv threaths
    wifi_sender_thread = threading.Thread(target=udp_client_send, args=(SERVER_IP, SERVER_PORT, PACKET_SIZE, NR_OF_PACKETS, PACKETS_PER_SEC,offset,wifi_sock,'wifi'))
    #wifi_sender_thread.daemon=True
    wifi_sender_thread.start()
    
    LTE_sender_thread = threading.Thread(target=udp_client_send, args=(SERVER_IP, SERVER_PORT, PACKET_SIZE, NR_OF_PACKETS, PACKETS_PER_SEC,offset,LTE_sock,'lte'))
    #LTE_sender_thread.daemon=True
    LTE_sender_thread.start()
    
    wifi_receiver_thread = threading.Thread(target=udp_client_receive, args=(UDP_RECEIVE_IP_wifi, UDP_RECEIVE_PORT_wifi,offset,wifi_sock,'wifi'))
    wifi_receiver_thread.daemon=True
    wifi_receiver_thread.start()

    LTE_receiver_thread = threading.Thread(target=udp_client_receive, args=(UDP_RECEIVE_IP_LTE, UDP_RECEIVE_PORT_LTE,offset,LTE_sock,'lte'))
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
 
 elif MODE=='downlink':
  sniff_wifi()
  sniff_lte()
  #send one packet by wifi interface to handshake with the server
  udp_client_send(SERVER_IP, SERVER_PORT, PACKET_SIZE, 1, PACKETS_PER_SEC,offset,wifi_sock,'wifi')
  #send one packet by LTE interface to handshake with the NAT
  udp_client_send(SERVER_IP, SERVER_PORT, PACKET_SIZE, 1, PACKETS_PER_SEC,offset,LTE_sock,'lte')
  #start receiver threaths
  wifi_receiver_thread = threading.Thread(target=udp_client_receive, args=(UDP_RECEIVE_IP_wifi, UDP_RECEIVE_PORT_wifi,offset,wifi_sock,'wifi'))
  #wifi_receiver_thread.daemon=True
  wifi_receiver_thread.start()
  LTE_receiver_thread = threading.Thread(target=udp_client_receive, args=(UDP_RECEIVE_IP_LTE, UDP_RECEIVE_PORT_LTE,offset,LTE_sock,'lte'))
  #LTE_receiver_thread.daemon=True
  LTE_receiver_thread.start()
  
  #close all threads
  #wifi_receiver_thread.join()
  #lte_receiver_thread.join()
  time.sleep(5)
  









    







