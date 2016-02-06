#tcpdump=read.csv("csv/tcpdump_500bytes_100packets100sec2016-01-25_113509_909.csv",header=FALSE, col.names = c("time sent","IP","interface"," ","dst","proto"," "," ","length","packet"))
#rcv=read.csv("csv/udp_oneway_500bytes_100packets100sec2016-01-25_113502_851.csv",header=FALSE, col.names = c("epoch time","status","packet","delay ","interface"))
rm=list(ls(all=TRUE))
directory='/Users/bahar/Documents/tcpdumpData/python/csv/2016-02-04_121805_107/'
tcpdumpfile=paste(directory,'tcpdump_1500bytes_2000packets100sec2016-02-04_121805_107.csv',sep="")
rcvfile=paste(directory,'udp_oneway_1500bytes_2000packets100sec2016-02-04_121747_845.csv',sep="")
tcpdump=read.csv(tcpdumpfile,header=FALSE, col.names = c("time sent","IP","interface"," ","dst","proto"," "," ","length","packet"))
rcv=read.csv(rcvfile,header=FALSE, col.names = c("epoch time","status","packet","delay ","interface"))


wifi_slots_sent=grep("wifi",tcpdump$interface)
wifi_slots_received=grep("wifi",rcv$interface)

wifi_packet_numbers_sent=tcpdump[wifi_slots_sent,10]
wifi_packet_numbers_received=rcv[wifi_slots_received,3]

lte_slots_sent=grep("lte",tcpdump$interface)
lte_slots_received=grep("lte",rcv$interface)

lte_packet_numbers_sent=tcpdump[lte_slots_sent,10]
lte_packet_numbers_received=rcv[lte_slots_received,3]

ordering_lte_packets=match(lte_packet_numbers_received,lte_packet_numbers_sent)
slot_delay_lte= lte_slots_received[]- lte_slots_sent[lte_packet_numbers_received]

ordering_wifi_packets=match(wifi_packet_numbers_received,wifi_packet_numbers_sent)
slot_delay_wifi= wifi_slots_received[ ]- wifi_slots_sent[wifi_packet_numbers_received]

#initialise the vector of s ent slots 
slots_sent <- data.frame (matrix(ncol=2,dim(tcpdump)[1])) 
slots_sent[lte_slots_sent,1]=-1
slots_sent[wifi_slots_sent,1]=-1
slots_sent[lte_slots_sent,2]='red'
slots_sent[wifi_slots_sent,2]='blue'

slots_rcvd <- data.frame (matrix(ncol = 2, nrow = dim(rcv)[1]))
slots_rcvd[lte_slots_received,1]=1
slots_rcvd[wifi_slots_received,1]=1
slots_rcvd[lte_slots_received,2]='red'
slots_rcvd[wifi_slots_received,2]='blue'

library(ggplot2)

setEPS()
postscript(paste(directory,"R/slots.eps",sep=""))
plot(lte_packet_numbers_received,slot_delay_lte,xlim = c(1,2000), ylim = c(-100,100),xlab='', ylab='', main='',col='black')
par(new=T)
plot(wifi_packet_numbers_received,slot_delay_wifi,xlim = c(1,2000), ylim = c(-100,100),xlab='packet number', ylab='received slot-sent slot',col='blue')
legend(70,50,c("LTE","WiFi"),pch=c(1,10),col=c("black","blue"))
dev.off()

setEPS()
postscript(paste(directory,"R/slots_send_rcv.eps",sep=""),width=15, height=10, paper = "special")
plot( slots_sent[,1]  ,xlim=c(1,dim(slots_sent)[1]),ylim = c(-1.1,1.1), xlab='slots',ylab='-1: sent, +1: rcv',main='RAT: blue:wifi red:lte', cex=5, col=slots_sent$X2, pch=0) 
#axis(1, xaxp=c(1, 4000, 19))
par(new=T)
plot( slots_rcvd[,1],xlim=c(1,dim(slots_sent)[1]),ylim = c(-1.1,1.1), cex=5, col=slots_rcvd$X2, pch=0)
dev.off
#setEPS()
#postscript(paste(directory,"R/slots_send_rcv.eps",sep=""))
#ggplot(tcpdump,aes(x=xvals,y=interface)) + geom_point(aes(colour=factor(interface), fill = factor(interface)), shape=21, size = 2)+scale_fill_manual(values=c("white", "white"))  + scale_colour_manual(values=c("blue", "cyan4"))+xlab("packet number")+ ylab("one way delay (sec)")
#xvals <-vector (mode='numeric', length=dim(tcpdump)[1])
#dev.off
