rm=list(ls(all=TRUE))

#load the libraries
library(DBI)
library(RSQLite)
library(ggplot2)


directory="/Users/bahar/Documents/tcpdumpData/python/csv/center/"
folder_list=list.files(path=directory, pattern ="2016-02-14_*", all.files = FALSE)
#dev.off()
quartz(width=10, height=20)
#par(mfrow = c(7, 5))
par(mar=c(4,4,4,1))
#par(oma=c(2,2,1,1))

ncolPlots = 3
n_plots<- ncolPlots *length(folder_list)
mat <- matrix(1:n_plots,ncol=3,byrow=TRUE)
layout(mat)
#layout(mat,rep.int(2, ncol(mat)),heights =c( rep.int(2, nrow(mat)-1), 3), TRUE)
layout.show(n_plots)
for (folder in folder_list) {
  striped_file <- strsplit(folder,split='_')
  flags <- do.call('rbind',lapply(striped_file,tail,n=2))
  platform <- flags[1,1]
  trx_mode <- flags[1,2]
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
    
    #create  databases
    con <- dbConnect(SQLite(), dbname = "sample_db.sqlite")
    #read received (one way delay) csv file into SQL database
    dbWriteTable(con, name="sample_table", value=rcvfile,  row.names=FALSE, header=FALSE, sep = ",",overwrite = TRUE)
    #read received (sent) csv file into SQL database
    dbWriteTable(con, name="sample_table2", value=tcpdumpfile,  row.names=FALSE, header=FALSE, sep = ",",overwrite = TRUE)
    #all data
    allRcvd <-  dbGetQuery(con, "SELECT * FROM sample_table")
    if (trx_mode=='downlink') {
     #strip rcvd wifi data
     wifiRcvd <- dbGetQuery(con, "SELECT * FROM sample_table where V5='wifi'")
     #strp rcvd lte data
     lteRcvd <- dbGetQuery(con, "SELECT * FROM sample_table where V5='lte'")
    }
    else if (trx_mode=='uplink') {
      #strip rcvd wifi data
      wifiRcvd <- dbGetQuery(con, "SELECT * FROM sample_table where V5=' wifi'")
      #strp rcvd lte data
      lteRcvd <- dbGetQuery(con, "SELECT * FROM sample_table where V5=' lte'")
    }
    
    
    #finding identical LTE and wifi packets
    wifi_recv_time=wifiRcvd[wifiRcvd$V3,1]
    lte_recv_time=lteRcvd[lteRcvd$V3,1]
    packet_match <- vector()
    for (i in 1:length(wifiRcvd$V3)) {
      matching_lte_ind=which(lteRcvd$V3 == wifiRcvd$V3[i])
      if (length(matching_lte_ind)!=0) {
        
        packet_match[i]= matching_lte_ind
      }
    }
    #rcvd time difference for identical lte/wifi packets
    #*********comment this out because I recorded time  with low precision in downlink**********
    #time_difference=wifiRcvd$V1[]-lteRcvd$V1[packet_match]
    
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
     hist(wifiRcvd$V4,breaks=30, col='blue', xlab = "delay (sec)", ylab = "Frequency",main="",xlim = c(0,max(allRcvd$V4)))
     hist(lteRcvd$V4,breaks=30, col='red',add=T)
    }
    else{
      hist(wifiRcvd$V4, breaks=30,col='blue', main="", xlab="",ylab = "Frequency",xlim = c(0,max(allRcvd$V4)))
      hist(lteRcvd$V4,breaks=30, col='red',add=T)
    }
    #ggplot hist
    #ggplot(allRcvd,aes(V4,fill= V5)) + geom_histogram(binwidth=0.001)+xlab("one way delay (sec)")
    #dev.off()
    #scatter
    #setEPS()
    #postscript(paste(folder_directory,"R/timeseries.eps",sep=""))
    #ggplot(allRcvd,aes(x=V3,y=V4)) + geom_point(aes(colour=factor(V5), fill = factor(V5)), shape=21, size = 2)+scale_fill_manual(values=c("white", "white"))  + scale_colour_manual(values=c("blue", "cyan4"))+xlab("packet number")+ ylab("one way delay (sec)")
    
    if (length(grep(folder,folder_list[length(folder_list)])) !=0) {
     plot(rcv$packet,rcv$delay,xlab="packet number", ylab="delay (sec)")
     points (wifiRcvd$V3,wifiRcvd$V4,main=" ",col='blue',cex=.5,pch=1)
     points(lteRcvd$V3,lteRcvd$V4,col='red',cex=.5,pch=1)
    }
    else {
      plot(rcv$packet,rcv$delay,xlab="", ylab="delay (sec)")
      points (wifiRcvd$V3,wifiRcvd$V4,main=" ",col='blue',cex=.5,pch=1)
      points (lteRcvd$V3,lteRcvd$V4,main=" ",col='red',cex=.5,pch=1)
    }
    
    
    # rcv time difference for identical LTE and wifi packets
    #*********comment this out because I recorded time  with low precision in downlink**********
    #if (length(grep(folder,folder_list[length(folder_list)])) !=0) { 
      #plot(time_difference, xlab='received wifi packets' , ylab='wifi rcv time- lte rcv time (s)' )
    #}
    #else{
      #plot(time_difference, xlab='' , ylab='wifi rcv time- lte rcv time (s)' )
    #}
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
    #find index of a rcvd/sent packet
    ordering_lte_packets=match(lte_packet_numbers_received,lte_packet_numbers_sent)
    slot_delay_lte= lte_slots_received[]- lte_slots_sent[lte_packet_numbers_received]
    ordering_wifi_packets=match(wifi_packet_numbers_received,wifi_packet_numbers_sent)
    slot_delay_wifi= wifi_slots_received[ ]- wifi_slots_sent[wifi_packet_numbers_received]
    
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
    #for (i in 1:dim(slots_rcvd)[1]){
      #slots_rcvd[i,3]=slots_sent[ grepl( rcv$interface[i],tcpdump$interface) & tcpdump$packet==rcv$packet[i] ,3]+ rcv$delay.[i]
      #print (i)
    #}
    
#     ##############plot lte wifi orders against time ###################
#     mtext(plot_title,side=3)
#     if (length(grep(folder,folder_list[length(folder_list)])) !=0) {
#       plot(slots_sent[,3], slots_sent[,1]  ,xlim=c(1,max(slots_sent[,3])),ylim = c(-1.1,1.1), xlab='time- 0: starting time',ylab="", cex=5, col=slots_sent$X2, pch=0,cex.lab=.1) 
#       par(new=T)
#       plot(slots_rcvd[,3], slots_rcvd[,1],xlim=c(1,max(slots_rcvd[,3])),ylim = c(-1.1,1.1), cex=5, col=slots_rcvd$X2, pch=0, xlab="",ylab="")
#       
#     }
#     else{
#       plot(slots_sent[,3], slots_sent[,1]  ,xlim=c(1,max(slots_sent[,3])),ylim = c(-1.1,1.1),xlab='',ylab='-1: sent, +1: rcv', cex=5, col=slots_sent$X2, pch=0,cex.lab=.1) 
#       par(new=T)
#       plot(slots_rcvd[,3], slots_rcvd[,1],xlim=c(1,max(slots_rcvd[,3])),ylim = c(-1.1,1.1), cex=5, col=slots_rcvd$X2, pch=0, xlab ="",ylab="")
#       
#     }
    
    ##############plot lte wifi orders against slot numbers (indices) ###################
    mtext(plot_title,side=3, cex=.8)
    temp=slots_sent[,1]
    temp[1]=1
    
    if (length(grep(folder,folder_list[length(folder_list)])) !=0) {
      plot(1:length(slots_sent[,1]),temp,xlab='slots',ylab="-1: sent, +1: rcv")
     points( slots_sent[,1]  ,xlim=c(1,max(slots_sent[,3])),ylim = c(-1.1,1.1) , cex=5, col=slots_sent$X2, pch=0) 
     points( slots_rcvd[,1],xlim=c(1,max(slots_rcvd[,3])),ylim = c(-1.1,1.1), cex=5, col=slots_rcvd$X2, pch=0)
     }
    else{
      plot(1:length(slots_sent[,1]),temp,ylab='-1: sent, +1: rcv',xlab="")
      points(slots_sent[,1]  ,xlim=c(1,max(slots_sent[,3])),ylim = c(-1.1,1.1), cex=5, col=slots_sent$X2, pch=0) 
      points( slots_rcvd[,1],xlim=c(1,max(slots_rcvd[,3])),ylim = c(-1.1,1.1), cex=5, col=slots_rcvd$X2, pch=0)
      
      }
  
    #disconnect data base
    dbDisconnect(con)
    
  }
  # remove previous direcotory variables
  rm(list=setdiff(ls(), c("folder","directory","folder_list")))
  
}
dev.copy2eps()
