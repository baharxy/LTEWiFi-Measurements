rm=list(ls(all=TRUE))

#load the libraries
library(DBI)
library(RSQLite)
library(ggplot2)
directory="/Users/bahar/Documents/tcpdumpData/python/csv/"
folder_list=list.files(path=directory, pattern = "2016-02-06*", all.files = FALSE)

quartz(width=10, height=8) # dimensions of plotting device are given in 7 x 7 inches by default
par(mfrow=c(8,3), omi=rep(0.5, 4)) # mfrow sets the number of rows and columns of plotting panels; omi (outer margin in inches) creates an external margin (in inches) around the entire plotting device - it has four values (bottom, left, top, right).

for (folder in folder_list) {
   
 
  folder_directory= paste(directory,folder,'/',sep="")
  tcpdumpfile=paste(folder_directory,list.files(path=folder_directory, pattern="tcpdump*"),sep='')
  rcvfile=paste(folder_directory,list.files(path=folder_directory, pattern="udp_oneway*"),sep='')
  channelfile=paste(folder_directory,list.files(path=folder_directory, pattern="channels*"),sep='')
  if (length(list.files(path=folder_directory, pattern="tcpdump*"))==0 |
      length(list.files(path=folder_directory, pattern="udp_oneway*"))==0 |
      length(list.files(path=folder_directory, pattern="channels*"))==0 ) { 
    print (c('folder',folder_directory,'has incomplete data'))
  }
  else{
    #create a database
    con <- dbConnect(SQLite(), dbname = "sample_db.sqlite")
    #read received (one way delay) csv file into SQL database
    dbWriteTable(con, name="sample_table", value=rcvfile,  row.names=FALSE, header=FALSE, sep = ",",overwrite = TRUE)
    #read received (sent) csv file into SQL database
    dbWriteTable(con, name="sample_table2", value=tcpdumpfile,  row.names=FALSE, header=FALSE, sep = ",",overwrite = TRUE)
    #all data
    allRcvd <-  dbGetQuery(con, "SELECT * FROM sample_table")
    #strip rcvd wifi data
    wifiRcvd <- dbGetQuery(con, "SELECT * FROM sample_table where V5=' wifi'")
    #strp rcvd lte data
    lteRcvd <- dbGetQuery(con, "SELECT * FROM sample_table where V5=' lte'")
    
    
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
    time_difference=wifiRcvd$V1[]-lteRcvd$V1[packet_match]
    
    #ggplot
    #hist
    #setEPS()
    postscript(paste(folder_directory,"R/histogram.eps",sep=""))
    ggplot(allRcvd,aes(V4,fill= V5)) + geom_histogram(binwidth=0.001)+xlab("one way delay (sec)")
    dev.off()
    #scatter
    #setEPS()
    postscript(paste(folder_directory,"R/timeseries.eps",sep=""))
    ggplot(allRcvd,aes(x=V3,y=V4)) + geom_point(aes(colour=factor(V5), fill = factor(V5)), shape=21, size = 2)+scale_fill_manual(values=c("white", "white"))  + scale_colour_manual(values=c("blue", "cyan4"))+xlab("packet number")+ ylab("one way delay (sec)")
    dev.off() 
    # rcv time difference for identical LTE and wifi packets
    #setEPS()
    postscript(paste(folder_directory,"R/reordering.eps",sep=""))
    plot(time_difference, xlab='received wifi packets' , ylab='wifi rcv time- lte rcv time (s)' )
    dev.off() 
    
    ###################### re ordering ###################
    
    # read files
    tcpdump=read.csv(tcpdumpfile,header=FALSE, col.names = c("time sent","IP","interface"," ","dst","proto"," "," ","length","packet"))
    rcv=read.csv(rcvfile,header=FALSE, col.names = c("epoch time","status","packet","delay ","interface"))
    
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
    
    #initialise the vector of s ent slots 
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
    for (i in 1:dim(slots_rcvd)[1]){
      slots_rcvd[i,3]=slots_sent[ grepl( rcv$interface[i],tcpdump$interface) & tcpdump$packet==rcv$packet[i] ,3]+ rcv$delay.[i]
      
    }
    
    
    
    #setEPS()
    postscript(paste(folder_directory,"R/slots.eps",sep=""))
    plot(lte_packet_numbers_received,slot_delay_lte,xlim = c(1,2000), ylim = c(-100,100),xlab='', ylab='', main='',col='black')
    par(new=T)
    plot(wifi_packet_numbers_received,slot_delay_wifi,xlim = c(1,2000), ylim = c(-100,100),xlab='packet number', ylab='received slot-sent slot',col='blue')
    legend(70,50,c("LTE","WiFi"),pch=c(1,10),col=c("black","blue"))
    dev.off()
    
    #setEPS()
    postscript(paste(folder_directory,"R/slots_send_rcv.eps",sep=""),width=15, height=10, paper = "special")
    plot( slots_sent[,1]  ,xlim=c(1,dim(slots_sent)[1]),ylim = c(-1.1,1.1), xlab='slots',ylab='-1: sent, +1: rcv',main='RAT: blue:wifi red:lte', cex=5, col=slots_sent$X2, pch=0) 
    #axis(1, xaxp=c(1, 4000, 19))
    par(new=T)
    plot( slots_rcvd[,1],xlim=c(1,dim(slots_sent)[1]),ylim = c(-1.1,1.1), cex=5, col=slots_rcvd$X2, pch=0)
    dev.off
    
    
    #setEPS()
    postscript(paste(folder_directory,"R/slots_send_rcv_time.eps",sep=""),width=15, height=10, paper = "special")
    plot(slots_sent[,3], slots_sent[,1]  ,xlim=c(1,max(slots_sent[,3])),ylim = c(-1.1,1.1), xlab='time- 0: starting time',ylab='-1: sent, +1: rcv',main='RAT: blue:wifi red:lte', cex=5, col=slots_sent$X2, pch=0) 
    #axis(1, xaxp=c(1, 4000, 19))
    par(new=T)
    plot(slots_rcvd[,3], slots_rcvd[,1],xlim=c(1,max(slots_rcvd[,3])),ylim = c(-1.1,1.1), cex=5, col=slots_rcvd$X2, pch=0)
    dev.off
    
    #setEPS()
    #postscript(paste(directory,"R/slots_send_rcv.eps",sep=""))
    #ggplot(tcpdump,aes(x=xvals,y=interface)) + geom_point(aes(colour=factor(interface), fill = factor(interface)), shape=21, size = 2)+scale_fill_manual(values=c("white", "white"))  + scale_colour_manual(values=c("blue", "cyan4"))+xlab("packet number")+ ylab("one way delay (sec)")
    #xvals <-vector (mode='numeric', length=dim(tcpdump)[1])
    #dev.off
    #disconnect data base
    dbDisconnect(con)
    
  }
  
  rm(list=setdiff(ls(), c("folder","directory")))
  
  
}

