#function that will print a scatterplot and correlation line for each company
N_Pos_Company <- function(Companyx){
  Companyx_Female <- subset(Companyx, Gender=="Female")
  Companyx_Male <- subset(Companyx, Gender=="Male")
  #printing correlation
  print(cor(Companyx$N_Pos, Companyx$Ten_Len, method = c("pearson", "kendall", "spearman")))
  #two rows with three plots each, first three: scatterplots, second three: boxplots
  par(mfrow=c(2,3))
  plot(Companyx$N_Pos, Companyx$Ten_Len, main="Correlation Number of Positions - Tenure: General",
       xlab="Number of Positions", ylab="Months", col="magenta1",pch=21)
  abline(lm(Companyx$Ten_Len~Companyx$N_Pos), col="blue")
  #
  plot(Companyx_Female$N_Pos, Companyx_Female$Ten_Len, main="Correlation Number of Positions - Tenure: Female",
       xlab="Number of Positions", ylab="Months", col="magenta1",pch=21)
  abline(lm(Companyx_Female$Ten_Len~Companyx_Female$N_Pos), col="blue")
  #
  plot(Companyx_Male$N_Pos, Companyx_Male$Ten_Len, main="Correlation Number of Positions - Tenure: Male",
       xlab="Number of Positions", ylab="Months", col="magenta1",pch=21)
  abline(lm(Companyx_Male$Ten_Len~Companyx_Male$N_Pos), col="blue")
  #boxplots
  boxplot(Companyx$Ten_Len, 
          col=c("magenta1"), 
          horizontal=TRUE)
  abline(v=c(9,24), col="red")
  boxplot(Companyx_Female$Ten_Len, 
          col=c("yellow"), 
          horizontal=TRUE)
  abline(v=c(9,24), col="red")
  boxplot(Companyx_Male$Ten_Len, 
          col=c("aquamarine1"), 
          horizontal=TRUE)
  abline(v=c(9,24), col="red")
}
#calling function
N_Pos_Company(Microsoft)


