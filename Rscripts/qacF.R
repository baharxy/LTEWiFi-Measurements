qacF <- function (x, y, conf.level = 0.95) {

    # ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
    bacf <- acf(x, lag.max=1000, plot = FALSE)
    bacfdf <- with(bacf, data.frame(lag, acf))
    
    bacf_2 <- acf(y, lag.max=1000, plot = FALSE)
    bacfdf_2 <- with(bacf_2, data.frame(lag, acf))
 
    q <- ggplot( bacfdf_2, aes( x = lag , y = acf, color='LTE') ) + geom_line (stat="identity", linetype = "dotdash")+
               ylab ("Autocorrelation of  Link Delays")  +xlab("Lag")
    q <- q + geom_line (   data= bacfdf,  aes( x = lag , y = acf, color='WiFi'),  stat="identity") 
     q <- q + scale_colour_manual(values=c("Firebrick","black"))
     
#     q <- q + geom_hline(yintercept = -ciline, color = "blue",
#                         size = 0.2)
#     q <- q + geom_hline(yintercept = ciline, color = "blue",
#                         size = 0.2)
#     q <- q + geom_hline(yintercept = 0, color = "red", size = 0.3)
#     
    q <- q +  theme_bw() +  theme(legend.title=element_blank())+ 
      theme(legend.justification=c(1,1), legend.position=c(1,1)) +
      guides(colour = guide_legend(override.aes = list(size=2)))+ 
      theme(legend.key=element_rect(fill='white'))+
      theme(legend.key = element_blank()) + theme(
      plot.background = element_blank()
      ,panel.grid.major = element_blank()
      ,panel.grid.minor = element_blank()
    ) 
    
    return(q)
    
}