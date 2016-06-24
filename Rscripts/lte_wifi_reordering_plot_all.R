rm=list(ls(all=TRUE))

#load the libraries
library(DBI)
library(RSQLite)
library(ggplot2)
library(zoo)
library(plyr)

directory="/Users/bahar/Documents/tcpdumpData/python/csv/androidDual/"
folder_list=list.files(path=directory, pattern ="2016-03-13_182703_075_LINUX_downlink", all.files = FALSE)
# folder_list=c(folder_list,list.files(path=directory, pattern ="2016-02-20_052906*", all.files = FALSE))
# folder_list=c(folder_list,list.files(path=directory, pattern ="2016-02-20_053110*", all.files = FALSE))
# folder_list=c(folder_list,list.files(path=directory, pattern ="2016-02-20_053813*", all.files = FALSE))


#dev.off()
# now by bahar
#quartz(width=28, height=2.5)
#par(mfrow = c(7, 5))
par(mar=c(4,4,4,1))
#par(oma=c(2,2,1,1))

ncolPlots = 7
n_plots<- ncolPlots *length(folder_list)
mat <- matrix(1:n_plots,ncol=7,byrow=TRUE)
layout(mat)
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
     rcv=read.csv(rcvfile,header=FALSE, col.names = c("epoch time","status","packet","delay ","interface"))
     allRcvd=rcv
     wifiRcvd  <- rcv[which(rcv$interface==' wifi'),]
     lteRcvd  <- rcv[which(rcv$interface==' lte'),]
     
    #create  databases
#      con <- dbConnect(SQLite(), dbname = "sample_db.sqlite")
#      #read received (one way delay) csv file into SQL database
#      dbWriteTable(con, name="sample_table", value=rcvfile,  row.names=FALSE, header=FALSE, sep = ",",overwrite = TRUE)
#      #read received (sent) csv file into SQL database
#      dbWriteTable(con, name="sample_table2", value=tcpdumpfile,  row.names=FALSE, header=FALSE, sep = ",",overwrite = TRUE)
#      #all data
#      allRcvd <-  dbGetQuery(con, "SELECT * FROM sample_table")
#     if (platform=='OSX') {
#     if (trx_mode=='downlink') {
#      #strip rcvd wifi data
#      wifiRcvd <- dbGetQuery(con, "SELECT * FROM sample_table where V5=' wifi'")
#      #strp rcvd lte data
#      lteRcvd <- dbGetQuery(con, "SELECT * FROM sample_table where V5=' lte'")
#     }
#     else if (trx_mode=='uplink') {
#       #strip rcvd wifi data
#       wifiRcvd <- dbGetQuery(con, "SELECT * FROM sample_table where V5=' wifi'")
#       #strp rcvd lte data
#       lteRcvd <- dbGetQuery(con, "SELECT * FROM sample_table where V5=' lte'")
#     }
#     }
#     else if (platform=='LINUX'){
#       if (trx_mode=='downlink') {
#         #strip rcvd wifi data
#         wifiRcvd <- dbGetQuery(con, "SELECT * FROM sample_table where V5='wifi'")
#         #strp rcvd lte data
#         lteRcvd <- dbGetQuery(con, "SELECT * FROM sample_table where V5='lte'")
#       }
#       else if (trx_mode=='uplink') {
#         #strip rcvd wifi data
#         wifiRcvd <- dbGetQuery(con, "SELECT * FROM sample_table where V5=' wifi'")
#         #strp rcvd lte data
#         lteRcvd <- dbGetQuery(con, "SELECT * FROM sample_table where V5=' lte'")
#       }
#     }
    
    
    
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
    # basic hist
    if (length(grep(folder,folder_list[length(folder_list)])) !=0) {
    if (length(wifiRcvd$delay.)!=0 ) {
      h1 <- hist((wifiRcvd$delay.),breaks=20, plot=FALSE)
      plot(h1$mids, h1$density, log="y", type='b',col='blue', xlab = "delay (sec)", ylab = "log scale- Frequency",main="")    
     }
      if (length(lteRcvd$delay.)!=0 ) { 
        
     h2<- hist((lteRcvd$delay.),breaks=20, plot=FALSE)
     par(new=TRUE)
     plot(h2$mids, h2$density, log="y", type='b', col='red',axes=FALSE, xlab='', ylab='')
      }
    }
    else{
      h1 <- hist((wifiRcvd$delay.),breaks="Scott", plot=FALSE)
      plot(h1$mids, h1$density, log="y", type='b',col='blue',xlab='', ylab = "log scale- Frequency",main="")    
      }
      if (length(lteRcvd$delay.)!=0 ) {
        h2<- hist((lteRcvd$delay.),breaks="Scott", plot=FALSE)
        par(new=TRUE)
        plot(h2$mids, h2$density, log="y", type='b', col='red',axes=FALSE, xlab='', ylab='')
      }
  }
  #ggplot hist
  #calculate mean of each group
  mu <- ddply(allRcvd, "interface", summarise, grp.mean=mean(delay.))
  dir.create(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,sep=""), showWarnings = TRUE, recursive = TRUE, mode = "0777")
  setEPS()
  postscript(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/histogram.eps', sep=""))
  p1 <- ggplot(allRcvd,aes(delay.,fill= interface, color=interface)) +
    geom_histogram(aes(y=..density..), position="identity", binwidth=0.00005) +
    geom_vline(data=mu, aes(xintercept=grp.mean, color=interface),linetype="dashed") +
    scale_color_grey()+ scale_fill_grey()   + theme_minimal() + theme(legend.position="top")
  p1 
  dev.off()
  #scatter time serries
  setEPS()
  postscript(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/timeSeries.eps',sep=""))
  p2 <- ggplot(allRcvd,aes(x=packet,y=delay.)) + geom_point(aes(colour=factor(interface), fill = factor(interface)), shape=21, size = 2)+
    scale_fill_manual(values=c("white", "white"))  + scale_colour_manual(values=c("blue", "cyan4"))+xlab("packet number")+ ylab("one way delay (sec)")
  p2 + scale_color_grey() + theme_minimal() +theme(legend.position="top")
  dev.off() 
  # scatter arrival times by interface 
  arrivalTime <- array(data=NA,dim=length(allRcvd$delay.))
  arrivalTime[1] <- 0
  arrivalTime[2:length(arrivalTime)] <- allRcvd$epoch.time -arrivalTime[0]
  allRcvd$arrivalTime<- arrivalTime
 
  #AutoCorrelation/CrossCorrelations
  if (length(grep(folder,folder_list[length(folder_list)])) !=0) {
    wifiRcvdDelayTs <- ts(wifiRcvd$delay.)
    lteRcvdDelayTs <- ts(lteRcvd$delay.)
    acf(wifiRcvdDelayTs,lag.max = 20, col='blue', xlabel='',ylabel='',main='',axes='false')
    par(new=TRUE)
    acf(lteRcvdDelayTs,lag.max = 20, col='red', xlabel='Lag',ylabel='ACF',main='')
    
    ccf(wifiRcvdDelayTs,lteRcvdDelayTs, col='blue', xlabel='Lag', ylabel='Cross-Correlation' ,main='')
   
  }
  else{
    wifiRcvdDelayTs <- ts(wifiRcvd$delay.)
    acf(wifiRcvdDelayTs,lag.max = 20, col='blue',xlabel='',ylabel='',axes='false',main='')
    lteRcvdDelayTs <- ts(lteRcvd$delay.)
    par(new=TRUE)
    acf(lteRcvdDelayTs,lag.max = 20, col='red', xlabel='',ylabel='ACF',main='')
    
    ccf(wifiRcvdDelayTs,lteRcvdDelayTs, col='blue', xlabel='',ylabel='Cross-Correlation',main='')
    
  }
#     time series
    if (length(grep(folder,folder_list[length(folder_list)])) !=0) {
     plot(rcv$packet,rcv$delay,xlab="packet number", ylab="delay (sec)")
      if (length(wifiRcvd$delay.)!=0 ) {
     points (wifiRcvd$packet,wifiRcvd$delay.,main=" ",col='blue',cex=.5,pch=1)
      }
      if (length(lteRcvd$delay.)!=0 ) {
     points(lteRcvd$packet,lteRcvd$delay.,col='red',cex=.5,pch=1)
      }
    }
    else {
      plot(rcv$packet,rcv$delay,xlab="", ylab="delay (sec)")
      if (length(wifiRcvd$delay.)!=0 ) {
      points (wifiRcvd$packet,wifiRcvd$delay.,main=" ",col='blue',cex=.5,pch=1)
      }
      if (length(lteRcvd$delay.)!=0 ) {
      points (lteRcvd$packet,lteRcvd$delay.,main=" ",col='red',cex=.5,pch=1)
      }
    }
    
    
    # rcv time difference for identical LTE and wifi packets
    #*********comment this out because I recorded time  with low precision in downlink**********
    
#     if (length(grep(folder,folder_list[length(folder_list)])) !=0) { 
#       plot(time_difference, xlab='received wifi packets' , ylab='wifi rcv time- lte rcv time (s)' )
#     }
#     else{
#       plot(time_difference, xlab='' , ylab='wifi rcv time- lte rcv time (s)' )
#     }
    #dev.off() 
    
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
    for ( wp in  1:length(wifi_slots_sent)) {
      
     if (!(wp %in% wifi_packet_numbers_received )){
       slot_delay_wifi[wp]<-NA
     }
      else{
        slot_delay_wifi[wp]=wifi_slots_sent[grep(wp, wifi_packet_numbers_sent)]-wifi_slots_received[grep(wp,wifi_packet_numbers_received)]
      }
    }
     
    for ( p in  1:length(lte_slots_sent)) {
      if (!(p %in% lte_packet_numbers_received )){
        slot_delay_lte[p]<-NA
      }
      else{
        slot_delay_lte[p]=lte_slots_sent[grep(p, lte_packet_numbers_sent)]-lte_slots_received[grep(p,lte_packet_numbers_received)]
      }
    }
    #find index of a rcvd/sent packet
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
