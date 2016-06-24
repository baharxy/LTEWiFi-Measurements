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
folder_list=list.files(path=directory, pattern ="2016-03-12_181328_105_LINUX_uplink", all.files = FALSE)
# folder_list=c(folder_list,list.files(path=directory, pattern ="2016-02-20_052906*", all.files = FALSE))
# folder_list=c(folder_list,list.files(path=directory, pattern ="2016-02-20_053110*", all.files = FALSE))
# folder_list=c(folder_list,list.files(path=directory, pattern ="2016-02-20_053813*", all.files = FALSE))


#dev.off()
# now by bahar
#quartz(width=28, height=2.5)
#par(mfrow = c(7, 5))
par(mar=c(4,4,4,1))
#par(oma=c(2,2,1,1))

# ncolPlots = 7
# n_plots<- ncolPlots *length(folder_list)
# mat <- matrix(1:n_plots,ncol=7,byrow=TRUE)
# layout(mat)
#layout(mat,rep.int(2, ncol(mat)),heights =c( rep.int(2, nrow(mat)-1), 3), TRUE)
# now by bahar
#layout.show(n_plots)
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
  #specify file directories to read
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
     rcv=read.csv(rcvfile,header=FALSE, col.names = c("epoch time","status","packet","delay ","interface"))
     allRcvd=rcv
     if (min(allRcvd$delay.) < 0) {
       allRcvd$delay. <-  allRcvd$delay.-min(allRcvd$delay.) + 0.001
     }
     #add column to received data
     #allRcvd$delay. <-  allRcvd$delay.-min(allRcvd$delay.)
     arrivalTime <- array(data=NA,dim=length(allRcvd$delay.))
     arrivalTime[1] <- 0
     arrivalTime[2:length(arrivalTime)] <- allRcvd$epoch.time[2:length(arrivalTime)] -allRcvd$epoch.time[1]
     allRcvd$arrivalTime<- arrivalTime
     wifiRcvd  <- allRcvd[which(allRcvd$interface==' wifi'),]
     lteRcvd  <- allRcvd[which(allRcvd$interface==' lte'),]
     
   
    
    
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
  
  
  p1 <- ggplot(allRcvd ) +     
    geom_histogram( aes(x=delay. ,y=..count../sum(..count..),  color=interface), binwidth=0.0005, position='identity', alpha=0.5 ) +  
    scale_fill_manual(values=c("white", "white" ))  + scale_colour_manual(values=c("Firebrick", "black")) + xlab("delay (sec)" )+ ylab("Density")
  
    #geom_vline(data=mu, aes(xintercept=grp.mean, color=interface),linetype="dashed" ) +
    #xlab("delay (sec)" )    #+  scale_color_grey() 
  # This line changes various aspects of the plot's appearance to be suited for black 
  # and white print.
  p1 <- p1 + theme_bw() + 
    theme(legend.title=element_blank())+ 
    theme(legend.justification=c(1,1), legend.position=c(1,1)) +
    guides(colour = guide_legend(override.aes = list(size=2)))+ 
    theme(legend.key=element_rect(fill='white'))+
    theme(legend.key = element_blank())+ theme(
    plot.background = element_blank()
    ,panel.margin = unit(0,"null")
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  ) +  xlab("delay (sec)" ) 
   
  # Now we remove the background grid.

  p1
  ggsave(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/histogram.eps', sep=""),dpi=300, p1)
  #dev.off()
  #scatter time serries
  #setEPS()
  allRcvd$interface <- as.character(allRcvd$interface)
  allRcvd$interface[ allRcvd$interface ==' wifi'] <- " WiFi"
  allRcvd$interface[ allRcvd$interface == ' lte' ] <- "LTE"
  
  p2 <- ggplot(allRcvd,aes(x=packet,y=delay.)) + geom_point(aes(  color =interface, shape=interface ) , size=1) +  scale_shape(solid=FALSE) +
    scale_colour_manual(values=c(  "black", "firebrick")) + xlab ("packet number") + ylab("one way delay (sec)")
  p2 <- p2 + theme_bw() + theme(legend.title=element_blank())+ 
    theme(legend.justification=c(1,1), legend.position=c(1,1)) +
    guides(colour = guide_legend(override.aes = list(size=2)))+ 
    theme(legend.key=element_rect(fill='white'))+
    theme(legend.key = element_blank()) +theme(
      plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
    ) 
  p2
  ggsave(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/timeSeries.eps',sep=""), dpi=300, p2)
  
  #scatter arrivals
  #setEPS()
  source("Rscripts/qacF.R")
  wifiRcvdDelayTs <- ts(wifiRcvd$delay.)
  lteRcvdDelayTs <- ts(lteRcvd$delay.)
  tptwifi <- turningpoint.test(wifiRcvdDelayTs)
  tptlte <- turningpoint.test(lteRcvdDelayTs)
  delayAuto <- qacF(wifiRcvdDelayTs,lteRcvdDelayTs)
  ggsave(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/Autoc.eps',sep=""),delayAuto)
  
#   delayAutolte <-  qacF(lteRcvdDelayTs, wifi=FALSE)
#   ggsave(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/lteAutoc.eps',sep=""),delayAutolte)
#   
  
  #AutoCorrelation/CrossCorrelations
  arrivalAuto <- qacF(wifiRcvd$arrivalTime, lteRcvd$arrivalTime)
  ggsave(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/ArrivalAutoc.eps',sep=""),arrivalAuto)
  
#   arrivalAutolte <-  qacF(lteRcvd$arrivalTime)
#   ggsave(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/lteArrivalAutoc.eps',sep=""),arrivalAutolte)
  

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
      
     if (!(wp %in% wifi_packet_numbers_received )){
       slot_delay_wifi[wp]<-NA
       WiFiArrivalTime[wp] <- NA
       WiFiDelayPacket[wp] <- NA
       
     }
      else{
        
        slot_delay_wifi[wp]=wifi_slots_sent[grep(wp, wifi_packet_numbers_sent)]-wifi_slots_received[grep(wp,wifi_packet_numbers_received)]
        WiFiArrivalTime[wp] <- wifiRcvd$arrivalTime[grep(wp, wifiRcvd$packet)]
        WiFiDelayPacket[wp] <-  wifiRcvd$delay.[grep(wp, wifiRcvd$packet)]
        
        }
    }
     
    for ( p in  1:length(lte_slots_sent)) {
      if (!(p %in% lte_packet_numbers_received )){
        slot_delay_lte[p]<-NA
        LTEArrivalTime[p] <- NA
        LTEDelayPacket[p] <- NA
      }
      else{
        slot_delay_lte[p]=lte_slots_sent[grep(p, lte_packet_numbers_sent)]-lte_slots_received[grep(p,lte_packet_numbers_received)]
        LTEArrivalTime[p] <- lteRcvd$arrivalTime[grep(p, lteRcvd$packet)]
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
                           'WiFiDelayPackets'=array(data=NA, dim = length(wifi_slots_sent)),'LTEDelayPackets'=array(data=NA, dim = length(wifi_slots_sent))
                           )
    packetDF$packet <- 1: length(wifi_slots_sent)
    packetDF$WiFiArrivalTimes <- WiFiArrivalTime
    packetDF$LTEArrivalTimes <- LTEArrivalTime
    packetDF$WiFiDelayPackets <- WiFiDelayPacket
    packetDF$LTEDelayPackets <- LTEDelayPacket
    packetDF$InterArrivals <- InterArrival
    packetDF$InterArrivalRevs <- InterArrivalRev
    
    # scatter plot of arrival times 
    p4 <- ggplot(packetDF,aes(x=WiFiArrivalTimes,y=LTEArrivalTimes)) + geom_line(size=0.3)+ ylim(min(allRcvd$arrivalTime),max(allRcvd$arrivalTime)) +
      xlab("WiFi Arrival Time (sec)")+ ylab("LTE Arrival Time (sec)")  + scale_fill_manual(values=c("white", "white"))  
    p4  <- p4 + theme_bw() + theme(
      plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
    )

    p4
    ggsave(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/WiFiLTEArrivals.eps',sep=""))
    
    
    # scatter plot of arrival times 
    x <- InterArrival[!is.na(InterArrival)]   
    parms <- fitdistr(x, "poisson")
    parms2 <- fitdistr(x, "normal")
    packetDF$norm <- dlnorm(packetDF$InterArrivals,mean=parms2$estimate[1],sd=parms2$estimate[2])
    
    packetDF$pois <- dpois(packetDF$InterArrivals,lambda=parms$estimate[1])
   
    
    
    p5 <- ggplot(packetDF,aes(x=InterArrivals )) +     
      geom_histogram(aes(y=..count../sum(..count..)),binwidth=0.001, position='identity', alpha=0.5 , fill="white", color='black') +
      #geom_line(aes(y = norm), col = "red") +
       xlab("Inter Arrival Time (sec)" ) +
         ylab("Density")
    
    #geom_vline(data=mu, aes(xintercept=grp.mean, color=interface),linetype="dashed" ) +
    #xlab("delay (sec)" )    #+  scale_color_grey() 
    # This line changes various aspects of the plot's appearance to be suited for black 
    # and white print.
    p5 <- p5 + theme_bw() + theme(
      plot.background = element_blank()
      ,panel.margin = unit(0,"null")
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
    ) +  xlab("InterArrivals (sec)" ) + ylab("Density")+  theme(legend.position="top") +
      theme(panel.border = element_rect(fill=NA, size=0.5)) 
    
    # Now we remove the background grid.
    
    p5
    ggsave(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/histogram2.eps', sep=""),dpi=300, p5)
    #find index of a rcvd/sent packetw
    ordering_lte_packets=match(lte_packet_numbers_received,lte_packet_numbers_sent)
    # lte packet loss
    lte_packets_lost=length(lte_packet_numbers_sent)-length(lte_packet_numbers_received)
    lte_packet_loss=(lte_packets_lost/length(lte_packet_numbers_sent) )*100
    slots_delay_lte= lte_slots_received[]- lte_slots_sent[lte_packet_numbers_received]
    ordering_wifi_packets=match(wifi_packet_numbers_received,wifi_packet_numbers_sent)
    # wifi packet loss
    wifi_packets_lost=length(wifi_packet_numbers_sent)-length(wifi_packet_numbers_received)
    wifi_packet_loss=(wifi_packets_lost/length(wifi_packet_numbers_sent) )*100
    slots_delay_wifi= wifi_slots_received[ ]- wifi_slots_sent[wifi_packet_numbers_received]
    #modify plot title with packet loss info 
    plot_title=paste(plot_title,'wifi loss', wifi_packet_loss,'lte loss', lte_packet_loss, sep = "_")
    #initialise the vector of sent slots 
    slots_sent <- data.frame (matrix(ncol=3,dim(tcpdump)[1])) 
    slots_sent[lte_slots_sent,1]=-1
    slots_sent[wifi_slots_sent,1]=-1
    slots_sent[lte_slots_sent,2]='red'
    slots_sent[wifi_slots_sent,2]='blue'
    slots_sent[1,3]=0
    initial_time=as.numeric(as.POSIXlt(tcpdump$time.sent[1], format='%H:%M:%OS', tz='UTC'))
    for (i in 1:dim(tcpdump)[1]){
      if (i>1){
        slots_sent[i,3]=as.numeric(as.POSIXlt(tcpdump$time.sent[i], format='%H:%M:%OS', tz='UTC'))- initial_time
      }
    }
    slots_rcvd <- data.frame (matrix(ncol = 3, nrow = dim(rcv)[1]))
    slots_rcvd[lte_slots_received,1]=1
    slots_rcvd[wifi_slots_received,1]=1
    slots_rcvd[lte_slots_received,2]='red'
    slots_rcvd[wifi_slots_received,2]='blue'
    #***************just for now***************************
    # first check if all the packets are sent (or number of sent packets is greater than number of received packets)
    if ( dim(tcpdump)[1] < dim(rcv)[1] ) {
      print('Error: number of sent packets is less than the number of received packets')
    }
    else {
    # for each rcvd packet, get the index of tcpdump sent packets that matches with the rcvd packet numbers
    # - from that get time of the sent packet in respect with the origin time (i.e. start of transmissions)
    # - then add the respective delay for that received packet to get the received time in respect to the origin time
    for (i in 1:dim(slots_rcvd)[1]){
      if (length(slots_sent[ grepl( rcv$interface[i],tcpdump$interface) & tcpdump$packet==rcv$packet[i] ,3])==0 ) {
        slots_rcvd[i,3] = 0 
        print ('tcpdump interrupted for some packets')
      }
      else {
       
       if (is.na(slots_rcvd[i,1])){} else{
      slots_rcvd[i,3]=slots_sent[ grepl( rcv$interface[i],tcpdump$interface) & tcpdump$packet==rcv$packet[i] ,3]+ rcv$delay.[i]
       } 
      }
    }
    }
    ##############plot lte wifi slot delays ###################
    mtext(plot_title,side=3)
    temp=slots_sent[,1]
    temp[1]=1
        if (length(grep(folder,folder_list[length(folder_list)])) !=0) {
          plot(slot_delay_wifi[1],ylim=c(-100,20),xlim=c(1,length(wifi_slots_sent)),xlab='packet number',ylab="slot received- slot sent")
          points( slot_delay_wifi ,xlim=c(1,length(wifi_slots_sent)) , col='blue') 
          points( slot_delay_lte,xlim=c(1,length(wifi_slots_sent)), col='red')
        }
        else{
          plot(slot_delay_wifi[1],ylim=c(-100,20),xlim=c(1,length(wifi_slots_sent)),ylab='slot received- slot sent',xlab="")
          points(slot_delay_wifi ,xlim=c(1,length(wifi_slots_sent)),  col='blue') 
          points(slot_delay_lte ,xlim=c(1,length(wifi_slots_sent)) ,  col='red')
        }
    ##############plot autocorrelation of lte wifi slot delays ###################
    if (length(grep(folder,folder_list[length(folder_list)])) !=0) {
      wifiSlotDelayTs <- ts(slot_delay_wifi[!is.na(slot_delay_wifi)])
      lteSlotDelayTs <- ts(slot_delay_lte[!is.na(slot_delay_lte)])
      acf(wifiSlotDelayTs,lag.max = 20, col='blue', xlabel='',ylabel='',main='',axes='false')
      par(new=TRUE)
      #remove NA (missing packets)
      acf(lteSlotDelayTs,lag.max = 20, col='red', xlabel='Lag',ylabel='ACF',main='')
      
      ccf(wifiSlotDelayTs, lteSlotDelayTs, col='blue', xlabel='Lag', ylabel='Cross-Correlation' ,main='')
      
    }
    else{
      wifiSlotDelayTs <- ts(slot_delay_wifi)
      acf(wifiSlotDelayTs,lag.max = 20, col='blue',xlabel='',ylabel='',axes='false',main='')
      lteRcvdDelayTs <- ts(slot_delay_lte)
      par(new=TRUE)
      acf(lteSlotDelayTs,lag.max = 20, col='red', xlabel='',ylabel='ACF',main='')
      
      ccf(wifiSlotDelayTs,lteSlotDelayTs, col='blue', xlabel='',ylabel='Cross-Correlation',main='')
      
    }
     ##############plot lte wifi orders against time ###################
    #   mtext(plot_title,side=3)
      temp=slots_sent[,1]
      temp[1]=1
#     if (length(grep(folder,folder_list[length(folder_list)])) !=0) {
#       plot(slots_sent[,3],temp,xlab='time (s)',ylab="-1: sent, +1: rcv")
#       points( slots_sent[,3],slots_sent[,1]  ,xlim=c(1,max(slots_sent[,3])),ylim = c(-1.1,1.1) , cex=5, col=slots_sent$X2, pch=0) 
#       points( slots_rcvd[,3], slots_rcvd[,1],xlim=c(1,max(slots_rcvd[,3])),ylim = c(-1.1,1.1), cex=5, col=slots_rcvd$X2, pch=0)
#     }
#     else{
#       plot(slots_sent[,3],temp,ylab='-1: sent, +1: rcv',xlab="")
#       points(slots_sent[,3],slots_sent[,1]  ,xlim=c(1,max(slots_sent[,3])),ylim = c(-1.1,1.1), cex=5, col=slots_sent$X2, pch=0) 
#       points(slots_rcvd[,3], slots_rcvd[,1],xlim=c(1,max(slots_rcvd[,3])),ylim = c(-1.1,1.1), cex=5, col=slots_rcvd$X2, pch=0)
#     }
    
    ##############plot lte wifi orders against slot numbers (indices) ###################
      #  mtext(plot_title,side=3, cex=.8)
#     temp=slots_sent[,1]
#     temp[1]=1
#     
#     if (length(grep(folder,folder_list[length(folder_list)])) !=0) {
#      plot(1:length(slots_sent[,1]),temp,xlab='slots',ylab="-1: sent, +1: rcv")
#      points( slots_sent[,1]  ,xlim=c(1,max(slots_sent[,3])),ylim = c(-1.1,1.1) , cex=5, col=slots_sent$X2, pch=0) 
#      points( slots_rcvd[,1],xlim=c(1,max(slots_rcvd[,3])),ylim = c(-1.1,1.1), cex=5, col=slots_rcvd$X2, pch=0)
#      }
#     else{
#       plot(1:length(slots_sent[,1]),temp,ylab='-1: sent, +1: rcv',xlab="")
#       points(slots_sent[,1]  ,xlim=c(1,max(slots_sent[,3])),ylim = c(-1.1,1.1), cex=5, col=slots_sent$X2, pch=0) 
#       points( slots_rcvd[,1],xlim=c(1,max(slots_rcvd[,3])),ylim = c(-1.1,1.1), cex=5, col=slots_rcvd$X2, pch=0)
#       
#       }
    
    #disconnect data base
    #dbDisconnect(con)
    
      # }
  # remove previous direcotory variables
  rm(list=setdiff(ls(), c("folder","directory","folder_list")))
  
}
dev.copy2eps()
