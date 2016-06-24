rm=list(ls(all=TRUE))

#load the libraries
library(DBI)
library(RSQLite)
library(ggplot2)
library(zoo)
library(plyr)
library(easyGgplot2)
library(MASS)
library(mixtools)
library(spgs)
directory="/Users/bahar/Documents/tcpdumpData/python/csv/androidDual/"
folder_list=list.files(path=directory, pattern ="2016-03-13_182703_075_LINUX_downlink", all.files = FALSE)
# folder_list=c(folder_list,list.files(path=directory, pattern ="2016-02-20_052906*", all.files = FALSE))
# folder_list=c(folder_list,list.files(path=directory, pattern ="2016-02-20_053110*", all.files = FALSE))
# folder_list=c(folder_list,list.files(path=directory, pattern ="2016-02-20_053813*", all.files = FALSE))

for (folder in folder_list) {
  striped_file <- strsplit(folder,split='_')
  flags <- do.call('rbind',lapply(striped_file,tail,n=3))
  if (flags[1,3]=='uplink' || flags[1,3]=='downlink') {
  platform <- flags[1,2]
  trx_mode <- flags[1,3]
  }
  else {
    platform <- flags[1,1]
    trx_mode <- flags[1,2]
    gso <- flags[1,3]
  }
  #spesify file directories to read
  folder_directory= paste(directory,folder,'/',sep="")
  tcpdumpfile=paste(folder_directory,list.files(path=folder_directory, pattern="tcpdump*"),sep='')
  rcvfile=paste(folder_directory,list.files(path=folder_directory, pattern="udp_oneway*"),sep='')
  channelfile=paste(folder_directory,list.files(path=folder_directory, pattern="channels*"),sep='')
  
  #if one of the files don't exit exit the for loop
  if (length(list.files(path=folder_directory, pattern="tcpdump*"))==0 |
      length(list.files(path=folder_directory, pattern="udp_oneway*"))==0 |
      length(list.files(path=folder_directory, pattern="channels*"))==0 ) { 
    print (c('folder',folder_directory,'has incomplete data'))
  }
  #otherwise do the measurements
  else{
    # read files
    if (trx_mode=='uplink') {
     tcpdump=read.csv(tcpdumpfile,header=FALSE, col.names = c("time sent","IP","interface"," ","dst","proto"," "," ","length","packet"))
    }
    else if (trx_mode=='downlink') {
     tcpdump=read.csv(tcpdumpfile,header=FALSE, col.names = c("time sent","IP","src"," ","interface","proto"," "," ","length","packet"))
    }
    # assuming that first packet is sent at 0 sec
    tcpdump$sentTimes= ( as.numeric(sapply( sapply(unlist(strsplit(toString(tcpdump$time.sent[1:length(tcpdump$time.sent)]),",",fixed= TRUE )), function(x) strsplit(x,":", fixed = TRUE)), function(x) unlist(x)) [2,] )-
                   as.numeric ( unlist( strsplit(toString(tcpdump$time.sent[1]),":",fixed= TRUE ) ) [2] ) ) * 60 +
      ( as.numeric (sapply( sapply(unlist(strsplit(toString(tcpdump$time.sent[1:length(tcpdump$time.sent)]),",",fixed= TRUE )), function(x) strsplit(x,":", fixed = TRUE)), function(x) unlist(x)) [3,] ) -
          as.numeric( unlist(strsplit(toString(tcpdump$time.sent[1]),":", fixed = TRUE))[3])  )
    
     rcv=read.csv(rcvfile,header=FALSE, col.names = c("epoch time","status","packet","delay ","interface"))
     allRcvd=rcv
     
     #add column to received data
     # 
     if (min(allRcvd$delay.) < 0) {
       allRcvd$delay. <-  allRcvd$delay.-min(allRcvd$delay.) + 0.001
     }
     arrivalTime <- array(data=NA,dim=length(allRcvd$delay.))
     arrivalTime[1] <- 0
     arrivalTime[2:length(arrivalTime)] <- allRcvd$epoch.time[2:length(arrivalTime)] -allRcvd$epoch.time[1]
     allRcvd$arrivalTime<- arrivalTime
     wifiRcvd  <- allRcvd[which(allRcvd$interface==' wifi'),]
     lteRcvd  <- allRcvd[which(allRcvd$interface==' lte'),]
    
     j=1
     
    lte_range=round (seq(1,length(lteRcvd$delay.),by= (length(lteRcvd$delay.)-1)/4 ))
     wifi_range=round(seq(1,length(wifiRcvd$delay.),by=(length(wifiRcvd$delay.)-1)/4))
     iters_wifi=length(wifi_range)
     iters_lte=length(lte_range)
     mean_interval_wifi <-vector('list',length=iters_wifi)
     sd_interval_wifi <-vector('list',length=iters_wifi)
     mean_interval_lte<-vector('list',length=iters_wifi)
     sd_interval_lte<-vector('list',length=iters_wifi)
     for (i in 1:(iters_wifi-1) ) {
       mean_interval_wifi[[i]]= mean(wifiRcvd$delay.[wifi_range[i]:wifi_range[i+1]])
       sd_interval_wifi[[i]]= sd(wifiRcvd$delay.[wifi_range[i]:wifi_range[i+1]])
     }
     mean_interval_wifi[[5]]=mean(wifiRcvd$delay.)
     sd_interval_wifi[[5]] =sd(wifiRcvd$delay.)
     for (i in 1:(iters_lte-1) ) {
       mean_interval_lte[[i]]= mean(lteRcvd$delay.[lte_range[i]:lte_range[i+1]])
       sd_interval_lte[[i]]= sd(lteRcvd$delay.[lte_range[i]:lte_range[i+1]])
     }
     mean_interval_lte[[5]]=mean(lteRcvd$delay.)
     sd_interval_lte[[5]] =sd(lteRcvd$delay.)
    
    #finding identical LTE and wifi packets
    wifi_recv_time=wifiRcvd[wifiRcvd$packet,1]
    lte_recv_time=lteRcvd[lteRcvd$packet,1]
    packet_match <- vector()
    for (i in 1:length(wifiRcvd$packet)) {
      matching_lte_ind=which(lteRcvd$packet == wifiRcvd$packet[i])
      if (length(matching_lte_ind)!=0) {
        
        packet_match[i]= matching_lte_ind
      }
    }
    #rcvd time difference for identical lte/wifi packets
    #*********comment this out because I recorded time  with low precision in downlink**********
    time_difference=wifiRcvd$epoch.time[]-lteRcvd$epoch.time[packet_match]
    
    ##########################Measurement bash properrties##########################################
    channel_data<-scan(channelfile, character(0),sep=',')
    transRate <- channel_data[grep ('Transmission rate:', channel_data)]
    wifiSSID <- gsub("[[:blank:]]", "",channel_data[grep('SSID:*',channel_data)[1]])
    wifiSignalStrength <- gsub("[[:blank:]]", "",channel_data[grep('RSSI: +',channel_data)[1]])
    
    cellular <- channel_data[grep(' [A-Z]+$',channel_data)[1]]
    if ( length( which(cellular==" NONE"))!=0 ) cellular <- 'LTE'
    cellulaSignalStrength <- channel_data[grep('RSSI:-[0-9]*',channel_data)[1]]
    plot_title=paste(wifiSSID,', ', wifiSignalStrength,'  ---  ',"Cellular Technology: ", cellular,', ', cellulaSignalStrength,'  --- ',  transRate,' ----', folder,sep='')
    ###################################Histogram###############################################
  }
  #ggplot hist
  #calculate mean of each group
  mu <- ddply(allRcvd, "interface", summarise, grp.mean=mean(delay.))
  dir.create(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,sep=""), showWarnings = TRUE, recursive = TRUE, mode = "0777")
  require(scales)
  mylog_trans <- 
  function (base = exp(1), from = 0) 
  {
    trans <- function(x) log(x, base) - from
    inv <- function(x) base^(x + from)
    trans_new("mylog", trans, inv, log_breaks(base = base), domain = c(base^from, Inf))
  }
  
  
  
    ###################### send and rcvd slots ###################################
    #seperate lte wifi slots
  
    wifi_slots_sent=grep("wifi",tcpdump$interface)
    wifi_slots_received=grep("wifi",rcv$interface)
    lte_slots_sent=grep("lte",tcpdump$interface)
    lte_slots_received=grep("lte",rcv$interface)
    # packet squence nummbers for each slot
    wifi_packet_numbers_sent=tcpdump[wifi_slots_sent,10]
    wifi_packet_numbers_received=rcv[wifi_slots_received,3]
    lte_packet_numbers_sent=tcpdump[lte_slots_sent,10]
    lte_packet_numbers_received=rcv[lte_slots_received,3]
    
    WiFiSentTime=NULL
    LTESentTime =  NULL
    slot_delay_wifi= NULL
    slot_delay_lte= NULL
    WiFiArrivalTime = NULL
    LTEArrivalTime = NULL
    WiFiDelayPacket = NULL
    LTEDelayPacket = NULL
    InterArrival= NULL
    InterArrival[1]=0
    InterArrivalRev= NULL
    InterArrivalRev[1]=0
    for ( wp in  1:length(wifi_slots_sent)) {
     WiFiSentTime [wp] <- tcpdump$sentTimes[which(tcpdump$packet == wp & tcpdump$interface ==' wifi ')]
     if (!(wp %in% wifi_packet_numbers_received )){
       slot_delay_wifi[wp]<-NA
       WiFiArrivalTime[wp] <- NA
       WiFiDelayPacket[wp] <- NA
       
     }
      else{
        
        slot_delay_wifi[wp]=wifi_slots_sent[grep(wp, wifi_packet_numbers_sent)]-wifi_slots_received[grep(wp,wifi_packet_numbers_received)]
        WiFiArrivalTime[wp] <-  wifiRcvd$delay.[grep(wp, wifiRcvd$packet)] +    WiFiSentTime [wp]   #wifiRcvd$arrivalTime[grep(wp, wifiRcvd$packet)]
        WiFiDelayPacket[wp] <-  wifiRcvd$delay.[grep(wp, wifiRcvd$packet)]
        
        }
    }
     
    for ( p in  1:length(wifi_slots_sent)) {
      LTESentTime [p] <- tcpdump$sentTimes[which(tcpdump$packet == p & tcpdump$interface==' lte ')]
      if (!(p %in% lte_packet_numbers_received )){
        slot_delay_lte[p]<-NA
        LTEArrivalTime[p] <- NA
        LTEDelayPacket[p] <- NA
      }
      else{
        slot_delay_lte[p]=lte_slots_sent[grep(p, lte_packet_numbers_sent)]-lte_slots_received[grep(p,lte_packet_numbers_received)]
        LTEArrivalTime[p] <- lteRcvd$delay.[grep(p, lteRcvd$packet)] +  LTESentTime [p] #lteRcvd$arrivalTime[grep(p, lteRcvd$packet)]
        LTEDelayPacket[p] <- lteRcvd$delay.[grep(p, lteRcvd$packet)]
        
        }
    }
    
    for ( pp in  1:length(wifi_slots_sent)) {
      
      if (!(pp %in% wifi_packet_numbers_received )){
        if (pp >1){
          InterArrival[pp] <- NA
          InterArrivalRev[pp] <- NA
        }
      }
      else{
        if (pp >1){
        InterArrival[pp]<- WiFiArrivalTime[pp]-LTEArrivalTime[pp-1]
        InterArrivalRev[pp]<- LTEArrivalTime[pp]-WiFiArrivalTime[pp-1]
        
        }
      }
    }
    
  
    
    packetDF <- data.frame("packet"=array(data=NA, dim = length(wifi_slots_sent)),'WiFiArrivalTimes'=array(data=NA, dim = length(wifi_slots_sent)), 'LTEArrivalTimes'=array(data=NA, dim = length(wifi_slots_sent)), 
                           'WiFiDelayPackets'=array(data=NA, dim = length(wifi_slots_sent)),'LTEDelayPackets'=array(data=NA, dim = length(wifi_slots_sent)) ,
                           'WiFiSentTimes'=array(data=NA, dim = length(wifi_slots_sent)),'LTESentTimes'=array(data=NA, dim = length(wifi_slots_sent))
                           )
    packetDF$packet <- 1: length(wifi_slots_sent)
    packetDF$WiFiArrivalTimes <- WiFiArrivalTime
    packetDF$LTEArrivalTimes <- LTEArrivalTime
    packetDF$WiFiDelayPackets <- WiFiDelayPacket
    packetDF$LTEDelayPackets <- LTEDelayPacket
    packetDF$InterArrivals <- InterArrival
    packetDF$InterArrivalRevs <- InterArrivalRev
    packetDF$WiFiSentTimes <- WiFiSentTime
    packetDF$LTESentTimes <- LTESentTime
    delaysDF <- packetDF [,4:5]
    #############   Scheduler assuming instantaneous feedback is available   ######################
    interfaceWithMinDelay <-  apply(delaysDF,1,which.min)
    ############# feedback reception times ###############################
    packetDF$feedbackreceivedWiFi <-  packetDF$WiFiSentTimes + 2* packetDF$WiFiDelayPackets
    packetDF$feedbackreceivedLTE <-  packetDF$LTESentTimes + 2* packetDF$LTEDelayPackets
    
    write.csv(file='delayDataIndiPizza.csv', packetDF)
    
    
    WiFiSentFB <- packetDF[,c('WiFiSentTimes','feedbackreceivedWiFi')]
    LTESentFB <- packetDF[,c('LTESentTimes','feedbackreceivedLTE')]
    
    wifisfeedbacks <- apply(WiFiSentFB , 1, function(x) which(x[1] > packetDF$feedbackreceivedWiFi))
    ltesfeedbacks <- apply(LTESentFB, 1, function(x) which(x[1] > packetDF$feedbackreceivedLTE))
    
    #function to map logical indexes to the interface names
    mapInface <- function(interface) {
     if (is.na(interface)) {
      if (runif(1,0,1)< 0.5) { out='wifi'} else { out='lte'}
     } else if (interface ==1) {
        out='wifi'
      } else if (interface == 2) {
        out='lte'
      }  
      return(out) 
    }
    # function to map logical indexes to the columns numbers
    mapToCol <- function(interface) {
      if (interface =='wifi') {
        out=2
      }else if (interface =='lte') {
        out=3
      }
      return(out) 
    }
    # offset time of the first packet (because we assumed packet arrivals start from 0 sec) assume that sending starts at 0 sec
    offsetArrival=rcv$delay.[1]
    
    # formalise scheduling based on interface in the data packet
    packetDF$SchedulePerfectFeedback <- sapply(interfaceWithMinDelay,mapInface) 
    ######## deterministic scheduling: send packets intermittently over each path ############
    packetDF$deterministicSchedule <- c( rep (c('wifi','lte'),length(wifi_slots_sent)/2))
    ####### Reorderings of different schedulings ###################
    ScheduledArrivalPerfect=array(data=NA, dim=dim(packetDF)[1])
    ScheduledArrivalDeterministic=array(data=NA, dim=dim(packetDF)[1])
    
    wifipp=0
    ltepp=0
    wifipd=0
    ltepd=0
    for ( psch in  1:dim(packetDF)[1] ){
     
      if (packetDF$SchedulePerfectFeedback[psch]=='wifi') { 
       wifipp <- wifipp + 1
       ScheduledArrivalPerfect[psch]=packetDF[psch,mapToCol(packetDF$SchedulePerfectFeedback[psch])]
       #assume that lost packets will be received correctly by retransmission in 1s !!
       if (is.na(ScheduledArrivalPerfect[psch])) {ScheduledArrivalPerfect[psch]= ScheduledArrivalPerfect[psch-1] + 1 }
       packetDF$ReorderingPerfect[psch]= max ( ScheduledArrivalPerfect[1:psch]) + offsetArrival - packetDF[psch,mapToCol(packetDF$SchedulePerfectFeedback[psch])+ 4 ] 
       
      } else if (packetDF$SchedulePerfectFeedback[psch]=='lte')  {
        ltepp <- ltepp +1
        ScheduledArrivalPerfect[psch]=packetDF[psch,mapToCol(packetDF$SchedulePerfectFeedback[psch])] 
        #assume that lost packets will be received correctly by retransmission in 1s !!
        if (is.na(ScheduledArrivalPerfect[psch])) {ScheduledArrivalPerfect[psch]= ScheduledArrivalPerfect[psch-1] + 1 }
        packetDF$ReorderingPerfect[psch]= max ( ScheduledArrivalPerfect[1:psch]) + offsetArrival - packetDF[psch,mapToCol(packetDF$SchedulePerfectFeedback[psch])+ 4 ] 
      }
      if (packetDF$deterministicSchedule[psch]=='wifi') {
        wifipd <- wifipd + 1
        ScheduledArrivalDeterministic[psch]=packetDF[psch,mapToCol(packetDF$deterministicSchedule[psch])] 
        #assume that lost packets will be received correctly by retransmission in 1s !!
        if (is.na(ScheduledArrivalDeterministic[psch])) {ScheduledArrivalDeterministic[psch]= ScheduledArrivalDeterministic[psch-1] + 1 }
        packetDF$ReorderingDiterministic[psch]= max ( ScheduledArrivalDeterministic[1:psch]) + offsetArrival - packetDF[psch,mapToCol(packetDF$deterministicSchedule)+ 4 ]
       } else if (packetDF$deterministicSchedule[psch]=='lte') { 
          ltepd <- ltepd +1
          ScheduledArrivalDeterministic[psch]=packetDF[psch,mapToCol(packetDF$deterministicSchedule[psch])] 
          if (is.na(ScheduledArrivalDeterministic[psch])) {ScheduledArrivalDeterministic[psch]= ScheduledArrivalDeterministic[psch-1] + 1 }
          packetDF$ReorderingDiterministic[psch]= max ( ScheduledArrivalDeterministic[1:psch]) + offsetArrival - packetDF[psch,mapToCol(packetDF$deterministicSchedule)+ 4 ]
          
      }
      
      
    }
  rm(list=setdiff(ls(), c("folder","directory","folder_list")))
  
}
dev.copy2eps()
