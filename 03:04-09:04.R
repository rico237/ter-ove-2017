library(ggplot2)
library(plyr)

GetPercentLabels <-function(val, digits = 2){
  pr<- paste(val, "%")
  return(pr)
}

BarStackedPlot2 <- function(df, aesX, aesF, legend.title = NULL, labelX = TRUE, labelF = TRUE) {
  print(df)
  x <- as.data.frame(ftable(df[ , c(aesX, aesF), drop=TRUE]))
  
  totFreq <- sum(x$Freq)
  ## Percentage label
  x$percentage <- 100 * x$Freq / totFreq
  x$percentage <-  GetPercentLabels(x$percentage, digits = 0)
  
  
  x <- ddply(x, aesX, transform, pos = cumsum(Freq) - (0.5 * Freq), top = cumsum(Freq))
  
  
  x$toplab <-  GetPercentLabels(100 * x$top / totFreq)
  
  
  m <- length(unique(x[,aesF]))
  
  print(x)
  ## exploit recycling
  x$toplab[ append(rep(TRUE, m-1), FALSE) ] <- ""
  
  p <- ggplot(x, aes_string(x = aesX, y = "Freq", fill = aesF)) + geom_bar(stat="identity") #+ theme_gdocs()
  
  if(labelF) {
    p <- p + geom_text(aes(y = pos, label = x$percentage, size = 6, position = "stack"), show.legend = FALSE)
  }
  
  if(labelX) {
    p <- p + geom_text(aes(y = x$top, label = x$toplab, size = 6, hjust = -0.25, vjust = -0.5, position = "stack", fontface = 2), show.legend = FALSE) + expand_limits( y = c(0,round(max(x$top)*1.05)))
  }
  
  if(is.null(legend.title)) {
    #p <- p + scale_fill_ptol()
  } else {
    #p <- p + scale_fill_ptol(name=legend.title)
  }
  
  p <- p + theme(legend.position="bottom", legend.direction="horizontal")
  
  return(p)
}

sa <- sample(c("oui", "non"), size=10,prob=c(0.7, 0.3), replace= TRUE)
c_sexe<-sample(c("h", "f"), 10, prob=c(0.7, 0.3), replace=TRUE) #Definition de sexes

tab <- table(sa, c_sexe)
das<-data.frame(tab/sum(tab))
das

testPlot <- BarStackedPlot2(df, df$sa, df$c_sexe)
testPlot

plot2 <- ggplot(df, aes(x=df$sa, y=df$c_sexe, fill=cumsum(df$Freq)))+ geom_bar(stat = "identity", position=position_dodge()) + 
  geom_text(aes(label=df$Freq), vjust=-1.6, size=3.5)
plot2

plot3 <- ggplot(das, aes(x=sa, y="Freq", fill=cumsum(Freq)))+ geom_bar(stat = "identity", position=position_dodge(0.9)) + 
  geom_text(aes(label=Freq), vjust=1.6,color="white", size=3.5)
plot3


plot <- ggplot(das, aes(x=sa, y=c_sexe, fill=Freq))+ geom_bar(stat = "identity", position=position_dodge()) + 
  geom_text(aes(label=Freq), vjust=-1.6, size=3.5)
plot

plot <- ggplot(das, aes(x=das$c_sexe, y=das$Freq, fill=factor(das$sa)))+ geom_bar(stat = "identity", position=position_dodge()) + 
  geom_text(aes(label=das$Freq), vjust=-1.6, size=3.5)+
  labs(title="Pourcentage des hommes et femmes possédant un emploi", x="Sexe", y="Pourcentage", fill="")
plot
