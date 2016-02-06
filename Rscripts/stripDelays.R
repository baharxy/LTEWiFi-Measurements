#to read delay measurement csv file and seperate out wifi/LTE data

#load the libraries
library(DBI)
library(RSQLite)
#get the file name
directory='/Users/bahar/Documents/tcpdumpData/python/csv/2016-02-04_091707_727/'
file_name = paste(directory,'udp_oneway_1500bytes_2000packets100sec2016-02-04_091701_554.csv',sep="")
#create a database
con <- dbConnect(SQLite(), dbname = "sample_db.sqlite")
#read csv file into SQL database
dbWriteTable(con, name="sample_table", value=file_name,  row.names=FALSE, header=FALSE, sep = ",",overwrite = TRUE)
#all data
myData9 <-  dbGetQuery(con, "SELECT * FROM sample_table")
#strip wifi data
wifiData9 <- dbGetQuery(con, "SELECT * FROM sample_table where V5=' wifi' ")
#strp lte data
lteData9 <- dbGetQuery(con, "SELECT * FROM sample_table where V5=' lte'")

wifi_recv_time=wifiData9[wifiData9$V3,1]
lte_recv_time=lteData9[lteData9$V3,1]

packet_match <- vector()
for (i in 1:length(wifiData9$V3)) {
  matching_lte_ind=which(lteData9$V3 == wifiData9$V3[i])
  if (length(matching_lte_ind)!=0) {

  packet_match[i]= matching_lte_ind
  }
}

time_difference=wifiData9$V1[]-lteData9$V1[packet_match]

     
    
dbDisconnect(con)

#plot histogram of the wifi and lte data data
#ordinary
#plot (wifiData$V3,wifiData$V4,xlim = c(1,100), ylim = c(0,0.1),main=" ", xlab="packet number", ylab="rtt delay (sec)",col='black')
#par(new=T)
#plot (lteData$V3,lteData$V4,xlim = c(1,100), ylim = c(0,0.1),main=" ", xlab="", ylab="",col='blue')
#hist(wifiData$V4, main = "Wifi:Packet size 500bytes", xlab = "wifi RTT (sec)", ylab = "Frequency")
#hist(lteData$V4, main = "LTE:Packet size 500bytes", xlab = "LTE RTT (sec)", ylab = "Frequency")

#ggplot
library(ggplot2)
#hist
setEPS()
postscript(paste(directory,"R/histogram.eps",sep=""))
ggplot(myData9,aes(V4,fill= V5)) + geom_histogram(binwidth=0.001)+xlab("one way delay (sec)")
dev.off()
#scatter
setEPS()
postscript(paste(directory,"R/timeseries.eps",sep=""))
ggplot(myData9,aes(x=V3,y=V4)) + geom_point(aes(colour=factor(V5), fill = factor(V5)), shape=21, size = 2)+scale_fill_manual(values=c("white", "white"))  + scale_colour_manual(values=c("blue", "cyan4"))+xlab("packet number")+ ylab("one way delay (sec)")
dev.off() 
#  rcv time difference for identical LTE and wifi packets
setEPS()
postscript(paste(directory,"R/reordering.eps",sep=""))
plot(time_difference, xlab='received wifi packets' , ylab='wifi rcv time- lte rcv time (s)' )
dev.off() 


