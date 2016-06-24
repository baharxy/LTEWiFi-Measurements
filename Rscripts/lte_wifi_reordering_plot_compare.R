rm=list(ls(all=TRUE))

#load the libraries
library(DBI)
library(RSQLite)
library(ggplot2)
library(zoo)
library(plyr)
library(easyGgplot2)
library(MASS)
library(gridExtra)
library(mixtools)
options(digits=10)

directory="/Users/bahar/Documents/tcpdumpData/python/csv/androidDual/"
folder_list=list.files(path=directory, pattern ="2016-03-12_182945_382_LINUX_downlink", all.files = FALSE)
folder_list=c(folder_list,list.files(path=directory, pattern ="2016-03-14_201627_697_LINUX_downlink", all.files = FALSE))
folder_list=c(folder_list,list.files(path=directory, pattern ="2016-04-21_155556_806_LINUX_downlink", all.files = FALSE))
folder_list=c(folder_list,list.files(path=directory, pattern ="2016-04-21_152414_316_LINUX_downlink", all.files = FALSE))

#dev.off()
# now by bahar
#quartz(width=28, height=2.5)
#par(mfrow = c(7, 5))
#par(mar=c(4,4,4,1))
#par(oma=c(2,2,1,1))

# ncolPlots = 7
# n_plots<- ncolPlots *length(folder_list)
# mat <- matrix(1:n_plots,ncol=7,byrow=TRUE)
# layout(mat)
#layout(mat,rep.int(2, ncol(mat)),heights =c( rep.int(2, nrow(mat)-1), 3), TRUE)
# now by bahar
#layout.show(n_plots)
c=1
info=NULL
wifiRcvdName=NULL
lteRcvdName=NULL
packetDFName=NULL
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
  info[c] = paste(strsplit(rcvfile,'_')[[1]][7], strsplit(rcvfile,'_')[[1]][8],sep=', ')
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
     
     #add column to received data
     allRcvd=rcv
     #add column to received data
     if (min(allRcvd$delay.) < 0) {
      allRcvd$delay. <-  allRcvd$delay.-min(allRcvd$delay.)
     }
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
    wifiMovingArrivals= data.frame(matrix(data=NA, nrow=length(wifi_slots_sent), ncol = 10))
    lteMovingArrivals=data.frame(matrix(data=NA, nrow=length(wifi_slots_sent), ncol = 10))
  
    nofpackets=4999
    
    for ( wp in  1:nofpackets) {
      
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
     
    for ( p in  1:nofpackets) {
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
    
    for ( pp in  1:nofpackets) {
      
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
      if (pp>1) {
        ref=max(1,pp-10)
        last=pp-1
        for (diffInd in ref:last) {
          wifiMovingArrivals[pp,pp-diffInd]= WiFiArrivalTime[pp]-WiFiArrivalTime[diffInd]
          lteMovingArrivals[pp,pp-diffInd]= LTEArrivalTime[pp]-LTEArrivalTime[diffInd]
        
        }
      }
    }
  
    packetDF <- data.frame("packet"=array(data=NA, dim = nofpackets),'WiFiArrivalTimes'=array(data=NA, dim = nofpackets), 'LTEArrivalTimes'=array(data=NA, dim = nofpackets), 
                           'WiFiDelayPackets'=array(data=NA, dim =nofpackets),'LTEDelayPackets'=array(data=NA, dim = nofpackets),
                           'InterArrivals'=array(data=NA, dim =nofpackets),'InterArrivalRevs'=array(data=NA, dim = nofpackets),
                            'wifiSlotDelays'=array(data=NA, dim = nofpackets), 'lteSlotDelays'=array(data=NA, dim = nofpackets))
    packetDF$packet[1:nofpackets] <- 1: nofpackets
    packetDF$WiFiArrivalTimes[1:length(WiFiArrivalTime)] <- WiFiArrivalTime
    packetDF$LTEArrivalTimes[1:length(LTEArrivalTime)] <- LTEArrivalTime
    packetDF$WiFiDelayPackets[1:length(WiFiDelayPacket)] <- WiFiDelayPacket
    packetDF$LTEDelayPackets [1:length(LTEDelayPacket)] <- LTEDelayPacket
    packetDF$InterArrivals [1:length(InterArrival)] <- InterArrival
    packetDF$InterArrivalRevs [1:length(InterArrivalRev)] <- InterArrivalRev
    packetDF$wifiSlotDelays[1:length(slot_delay_wifi)] <- slot_delay_wifi
    packetDF$lteSlotDelays[1:length(slot_delay_lte)] <- slot_delay_lte
   
    
    packetDF[,10:19] <- wifiMovingArrivals
    packetDF[,20:29 ] <- lteMovingArrivals
    wifiRcvdName[c] <- paste ("wifiRcvd", c, sep="")
    lteRcvdName[c] <-paste("lteRcvd",c,sep="")
    packetDFName[c] <-paste("packetDF",c,sep="")
  
    assign(wifiRcvdName[c], wifiRcvd)
    assign(lteRcvdName[c], lteRcvd)
    assign(packetDFName[c],packetDF)
    c<- c+1
    rm(list=setdiff(ls(), c("folder","directory","folder_list","c","info", "wifiRcvdName", "lteRcvdName","packetDFName", 
                            wifiRcvdName, lteRcvdName,packetDFName)))
  
}
###########################compare plots###############################################
############## Delay distribution#################
#plot block data
# source("Rscripts/multiplot.R")
# source("Rscripts/ggplotDensity.R")
# plotMoving <- function (df,numb) {
#     plots <- list()
#     nofp=1
#     for (s in  1:10) {
#       assign("lteplotName",paste("ltep",s,sep=""))
#       assign("wifiplotName",paste("wifip",s,sep=""))
#       assign("wifiData",paste("X",s,sep=""))
#       assign("lteData",paste("X",s,'.1',sep=""))
#       
#       ltep <-  ggplotDensity ( DF= df,column= lteData, plotName= lteplotName, rown=s, pathn='lte' ) 
#       wifip <-  ggplotDensity ( DF= df,column= wifiData, plotName= wifiplotName, rown=s, pathn='wifi' ) 
#       plots[[nofp]] <- ltep
#       plots[[nofp+1]] <-wifip
#       nofp <- nofp+2
#       
#     }
#     
#    # postscript(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/MovingArrivals',numb,'.eps', sep=""), paper="special", width = 20,height=50)
#      multiplot(plotlist=plots, cols=2, layout= matrix(1:20,ncol=2,byrow=TRUE))
#    #  dev.off()
# }
#   dflist <- list(packetDF1,packetDF2,packetDF3, packetDF4,packetDF5,packetDF6)
#   for (j in 1:length(folder_list)) {
#    plotMoving(dflist[[j]],j)
#   }

#plot in grid
###########################
#lte
# lteplot <- function (dataf, info, last, axt) {
#   if (last==0) {
#   l <- ggplot(dataf) +     
#     geom_histogram( color="black", fill="white", aes(x=delay. , y=..count../sum(..count..)), binwidth=0.0005, position='identity', alpha=0.5 ) + 
#     ylab ("") + xlab ("") + main (info)
#   }
#   else
#   {
#     l <- ggplot(dataf) +     
#       geom_histogram( color="black", fill="white", aes(x=delay. , y=..count../sum(..count..)), binwidth=0.0005, position='identity', alpha=0.5 ) + 
#       ylab ("") + xlab (axt) + main (info)
#   }
#   l <- l + theme_bw() + theme(
#     plot.background = element_blank()
#     ,panel.margin = unit(0,"null")
#     ,panel.grid.major = element_blank()
#     ,panel.grid.minor = element_blank()
#   ) + theme(panel.border = element_rect(fill=NA, size=0.5))
#   return (l)
# }
# 
# # Now we remove the background grid.
# 
# l1 <- lteplot(lteRcvd1,info[1],0,'')  
# l2 <- lteplot(lteRcvd2,info[2],0, '')  
# l3 <- lteplot(lteRcvd3,info[3],0, '')  
# l4 <- lteplot(lteRcvd4,info[4],0, '')  
# l5 <- lteplot(lteRcvd5,info[5],0, '')  
# l6 <- lteplot(lteRcvd6,info[6],1, 'LTE delay (s)') 
# 
# w1 <- lteplot(wifiRcvd1,info[1],0, '')  
# w2 <- lteplot(wifiRcvd2,info[2],0, '')  
# w3 <- lteplot(wifiRcvd3,info[3],0, '')  
# w4 <- lteplot(wifiRcvd4,info[4],0, '')  
# w5 <- lteplot(wifiRcvd5,info[5],0, '')  
# w6 <- lteplot(wifiRcvd6,info[6],1, 'WiFi delay (s)') 
# g1<- grid.arrange(l1, w1, l2,w2,l3,w3, l4,w4,l5,w5, ncol=2)
# 
# ggsave(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/LTEWiFiDownlinkhistograms.eps', sep=""),dpi=300,g1)

#Arrival Times Distribution
# interArrivalPlots <- function (dataf, info, last, axt) {
#     pint <-  ggplot(data,aes(x=InterArrivals )) +     
#       geom_histogram(color="black", fill="white", aes(y=..count../sum(..count..)),binwidth=0.001, position='identity', alpha=0.5 ) +
#       xlab ("")+ ylab ("density")
#     pint <- pint + theme_bw() + theme(
#       plot.background = element_blank()
#       ,panel.margin = unit(0,"null")
#       ,panel.grid.major = element_blank()
#       ,panel.grid.minor = element_blank()
#     ) +  theme(panel.border = element_rect(fill=NA, size=0.5))
#     return(pint)
#  }
# p5 <- interArrivalPlots(packetDF1,info[1],0,'')  
# p6 <- interArrivalPlots(packetDF2,info[2],0, '')  
# p7 <- interArrivalPlots(packetDF3,info[3],0, '')  
# 
# p8 <- interArrivalPlots(packetDF4,info[4],0,'')  
# p9 <- interArrivalPlots(packetDF5,info[5],0, '')  
# p10 <- interArrivalPlots(packetDF6,info[6],0, '')  
# g2 <- grid.arrange(p5,p6,p7,p8,p9,p10, ncol=1)
# 
# ggsave(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/InterArrivals.eps', sep=""),dpi=300,g2)

#slot delays

#  <- ggplot(packetDF1,aes(x=wifiSlotDelays )) +     
#   geom_histogram(color="black", fill="white", aes(y=..count../sum(..count..)),binwidth=0.0005, position='identity', alpha=0.5 ) +
#   xlab ("") + ylab ("")
# 
# wd1 <- wd1 + theme_bw() + theme(
#   plot.background = element_blank()
#   ,panel.margin = unit(0,"null")
#   ,panel.grid.major = element_blank()
#   ,panel.grid.minor = element_blank()
# ) +  theme(panel.border = element_rect(fill=NA, size=0.5)) 
# 
# 
# wd2 <- ggplot(packetDF2,aes(x=wifiSlotDelays )) +     
#   geom_histogram(color="black", fill="white", aes(y=..count../sum(..count..)),binwidth=0.0005, position='identity', alpha=0.5 ) + 
#    xlab ("") + ylab ("density")
# wd2 <- wd2 + theme_bw() + theme(
#   plot.background = element_blank()
#   ,panel.margin = unit(0,"null")
#   ,panel.grid.major = element_blank()
#   ,panel.grid.minor = element_blank()
# ) + theme(panel.border = element_rect(fill=NA, size=0.5)) 
# 
# wd3 <- ggplot(packetDF3,aes(x=wifiSlotDelays )) +     
#   geom_histogram(color="black", fill="white", aes(y=..count../sum(..count..)),binwidth=0.0005, position='identity', alpha=0.5 ) +
#   xlab ("WiFi slot delays (sec)" ) + ylab ("")
# 
# wd3 <- wd3 + theme_bw() + theme(
#   plot.background = element_blank()
#   ,panel.margin = unit(0,"null")
#   ,panel.grid.major = element_blank()
#   ,panel.grid.minor = element_blank()
# ) + theme(panel.border = element_rect(fill=NA, size=0.5)) 
# 
# ld1 <- ggplot(packetDF1,aes(x=lteSlotDelays )) +     
#   geom_histogram(color="black", fill="white", aes(y=..count../sum(..count..)),binwidth=0.0005, position='identity', alpha=0.5 ) +
#   xlab (" ") + ylab ("")
# 
# ld1 <- ld1 + theme_bw() + theme(
#   plot.background = element_blank()
#   ,panel.margin = unit(0,"null")
#   ,panel.grid.major = element_blank()
#   ,panel.grid.minor = element_blank()
# ) +  theme(panel.border = element_rect(fill=NA, size=0.5)) 
# 
# 
# ld2 <- ggplot(packetDF2,aes(x=lteSlotDelays )) +     
#   geom_histogram(color="black", fill="white", aes(y=..count../sum(..count..)),binwidth=0.0005, position='identity', alpha=0.5 ) +
#   xlab (" ") + ylab ("density")
# ld2 <- ld2 + theme_bw() + theme(
#   plot.background = element_blank()
#   ,panel.margin = unit(0,"null")
#   ,panel.grid.major = element_blank()
#   ,panel.grid.minor = element_blank()
# ) + theme(panel.border = element_rect(fill=NA, size=0.5)) 
# 
# ld3 <- ggplot(packetDF3,aes(x=lteSlotDelays )) +     
#   geom_histogram(color="black", fill="white", aes(y=..count../sum(..count..)),binwidth=0.0005, position='identity', alpha=0.5 ) +
#   xlab ("LTE slot delays (sec)" ) + ylab ("")
# 
# ld3 <- ld3 + theme_bw() + theme(
#   plot.background = element_blank()
#   ,panel.margin = unit(0,"null")
#   ,panel.grid.major = element_blank()
#   ,panel.grid.minor = element_blank()
# ) + theme(panel.border = element_rect(fill=NA, size=0.5)) 
# 
# g4 <- grid.arrange(ld1,wd1,ld2,wd2,ld3,wd3, ncol=2)
# 
# 
# ggsave(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/SlotDelays.eps', sep=""),dpi=300,g4)

# scatter plot of arrival times 
d1<- ggplot(packetDF1,aes(x=WiFiDelayPackets,y=LTEDelayPackets)) + geom_point(size=0.8, shape=21)+
  xlab("WiFi Delay (sec)- (a)") + ylab ("LTE Delay (sec)")  + scale_fill_manual(values=c("white", "white"))  
d1  <- d1 + theme_bw() + theme(
  plot.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
)

d2<- ggplot(packetDF2,aes(x=WiFiDelayPackets,y=LTEDelayPackets)) + geom_point(size=0.8,shape=21)+
  xlab("WiFi Delay (sec)- (b)") + ylab ("LTE Delay (sec)") +  scale_fill_manual(values=c("white", "white"))  
d2  <- d2 + theme_bw() + theme(
  plot.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
)

d3<- ggplot(packetDF3,aes(x=WiFiDelayPackets,y=LTEDelayPackets)) + geom_point(size=0.8, shape=21)+
  xlab("WiFi Delay (sec)- (c)") + ylab ("LTE Delay (sec)") + scale_fill_manual(values=c("white", "white"))  
d3  <- d3 + theme_bw() + theme(
  plot.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
)
d4<- ggplot(packetDF4,aes(x=WiFiDelayPackets,y=LTEDelayPackets)) + geom_point(size=0.8, shape=21)+
  xlab("WiFi Delay (sec)- (d)") + ylab ("LTE Delay (sec)") + scale_fill_manual(values=c("white", "white"))  
d4  <- d4 + theme_bw() + theme(
  plot.background = element_blank()
  ,panel.grid.major = element_blank()
  ,panel.grid.minor = element_blank()
)


g3<-grid.arrange(d1,d2,d3,d4 ,ncol=2)

ggsave(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/WiFiLTEArrivals.eps',sep=""), g3)

# ##################################
# #function to plot normal components of the mixture
# plot.normal.components <- function(mixture,component.number,...) {
#   curve(mixture$lambda[component.number] *
#           dnorm(x,mean=mixture$mu[component.number],
#                 sd=mixture$sigma[component.number]), add=TRUE, ...)
# }
# 
# 
# # function to  produce CDF
# pnormmix <- function(x,mixture) {
#   lambda <- mixture$lambda
#   k <- length(lambda)
#   pnorm.from.mix <- function(x,component) {
#     lambda[component]*pnorm(x,mean=mixture$mu[component],
#                             sd=mixture$sigma[component])
#   }
#   pnorms <- sapply(1:k,pnorm.from.mix,x=x)
#   return(rowSums(pnorms))
# }
# #fit mixure model to data
# ltedelaydata=lteRcvd1$delay.
# mixmdl=normalmixEM(ltedelaydata)
# plot(mixmdl,which=2)
# lines(density(ltedelaydata),lty=2,lwd=2)
# 
# ltedelaydata2=lteRcvd2$delay.
# mixmdl2=normalmixEM(ltedelaydata2)
# plot(mixmdl2,which=2)
# lines(density(ltedelaydata2),lty=2,lwd=2)
# 
# ltedelaydata3=lteRcvd3$delay.
# mixmdl3=normalmixEM(ltedelaydata3)
# plot(mixmdl3,which=2)
# lines(density(ltedelaydata3),lty=2,lwd=2)
# 
# #plot  mixtures for lte delay
# postscript(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/LTEmixturesDL.eps',sep=""), width=6, height=2)
# par(mfrow = c(1, 3), omi=rep(0.5, 4))
# plot(mixmdl, which=2)
#  lines(density(ltedelaydata),lty=2,lwd=2)
# plot(mixmdl2,which=2)
#  lines(density(ltedelaydata2),lty=2,lwd=2) 
# plot(mixmdl3,which=2)
#  lines(density(ltedelaydata3),lty=2,lwd=2)  
# dev.off() 
# 
# 
# postscript(paste("/Users/bahar/Documents/tcpdumpData/python/RPlots/",folder,'/LTEmixturesSamples.eps',sep=""), width=6, height=2)
# par(mfrow = c(1,3)) 
# distinct.ltedelaydata <- sort(unique(ltedelaydata))
# tcdfs <- pnormmix(distinct.ltedelaydata,mixture=mixmdl)
# ecdfs <- ecdf(ltedelaydata2)(distinct.ltedelaydata)
# plot(tcdfs,ecdfs,xlab="Theoretical CDF",ylab="Empirical CDF",xlim=c(0,1),
#      ylim=c(0,1), type='o', pch=1, cex=.1)
# abline(0,1)
# 
# distinct.ltedelaydata2 <- sort(unique(ltedelaydata2))
# tcdfs <- pnormmix(distinct.ltedelaydata2,mixture=mixmdl2)
# ecdfs <- ecdf(ltedelaydata2)(distinct.ltedelaydata2)
# plot(tcdfs,ecdfs,xlab="Theoretical CDF",ylab="Empirical CDF",xlim=c(0,1),
#      ylim=c(0,1), type='o', pch=1, cex=.1)
# abline(0,1)
# 
# distinct.ltedelaydata3 <- sort(unique(ltedelaydata3))
# tcdfs <- pnormmix(distinct.ltedelaydata3,mixture=mixmdl3)
# ecdfs <- ecdf(ltedelaydata3)(distinct.ltedelaydata3)
# plot(tcdfs,ecdfs,xlab="Theoretical CDF",ylab="Empirical CDF",xlim=c(0,1),
#      ylim=c(0,1), type='o', pch=1, cex=.1)
# abline(0,1)
# 
# dev.off()
