ggplotDensity <- function( DF,column,plotName,rown ,pathn) {
  if ( rown ==10 & pathn=='lte' ) {
    plotName <- ggplot(DF, aes( y=..count../sum(..count..))) +
      geom_histogram( color="black", fill="white", aes_string(x=column),  binwidth=0.0005, position='identity', alpha=0.5 ) + 
      ylab (paste( rown , "slot",sep="")) + xlab ("Inter Arrival (s) - LTE Path")
  }
  else if ( rown ==10 & pathn=='wifi' ) {
    plotName <- ggplot(DF, aes( y=..count../sum(..count..))) +
      geom_histogram( color="black", fill="white", aes_string(x=column),  binwidth=0.0005, position='identity', alpha=0.5 ) + 
      ylab ("") + xlab ("Inter Arrival (s) - WiFi Path")
  }
  else if (pathn=='lte' ) {
    plotName <- ggplot(DF, aes( y=..count../sum(..count..))) +
      geom_histogram( color="black", fill="white", aes_string(x=column),  binwidth=0.0005, position='identity', alpha=0.5 ) + 
      ylab (paste( rown , "slot",sep="")) + xlab ("")
  }
  else  {
    plotName <- ggplot(DF, aes( y=..count../sum(..count..))) +
      geom_histogram( color="black", fill="white", aes_string(x=column),  binwidth=0.0005, position='identity', alpha=0.5 ) + 
      ylab ("") + xlab ("")
  }
  plotName <- plotName + theme_bw() + theme(
    plot.background = element_blank()
    ,panel.margin = unit(0,"null")
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
  ) + theme(panel.border = element_rect(fill=NA, size=0.5)) 
  return(plotName)
}