#installing plotrix library
install.packages("plotrix")
Dataset2_Reduced
library(plotrix) 
#1 comparsison overall_rating___________________________________
#histogram comparing ratings for all 5 companies
multhist(list(facebook$Overall_Ratings, apple$Overall_Ratings, google$Overall_Ratings, 
              amazon$Overall_Ratings, microsoft$Overall_Ratings), breaks = seq(0.5,5.5), freq=FALSE, 
         col=c("dodgerblue3","darkorchid1",  "firebrick1", "chartreuse3", "darkorange"),
         main="Overall Ratings")
legend("topleft", c("Facebook", "Apple", "Google", "Amazon", "Microsoft"),
       col=c("dodgerblue3","darkorchid1",  "firebrick1", "chartreuse3", "darkorange"), pch=19)

#calculating the mean rating for each company
mean(facebook$Overall_Ratings)
mean(apple$Overall_Ratings)
mean(google$Overall_Ratings)
mean(amazon$Overall_Ratings)
mean(microsoft$Overall_Ratings)

#2 comparsison career_opportunities___________________________________
#calculating the mean rating for each company
mean(facebook$Career_Opportunities)
mean(apple$Career_Opportunities)
mean(google$Career_Opportunities)
mean(amazon$Career_Opportunities)
mean(microsoft$Career_Opportunities)
#comparing histogram to scatterplots from insight 2 (dataset1!)
par(mfrow=c(1,3))
multhist(list(facebook$Career_Opportunities, apple$Career_Opportunities, google$Career_Opportunities, 
              amazon$Career_Opportunities, microsoft$Career_Opportunities), breaks = seq(0.5,5.5), freq=FALSE, 
         col=c("dodgerblue3","darkorchid1",  "firebrick1", "chartreuse3", "darkorange"),
         main="Career Opportunities")
legend("topleft", c("Facebook", "Apple", "Google", "Amazon", "Microsoft"),
       col=c("dodgerblue3","darkorchid1",  "firebrick1", "chartreuse3", "darkorange"), pch=19)

#Microsoft
plot(Microsoft$N_Pos, Microsoft$Ten_Len, main="Correlation Number of Positions - Tenure: Microsoft",
     xlab="Number of Positions", ylab="Months", col="darkorange",pch=21)
abline(lm(Microsoft$Ten_Len~Microsoft$N_Pos), col="black")
#Amazon
plot(Amazon$N_Pos, Amazon$Ten_Len, main="Correlation Number of Positions - Tenure: Amazon",
     xlab="Number of Positions", ylab="Months", col="chartreuse3",pch=21)
abline(lm(Amazon$Ten_Len~Amazon$N_Pos), col="black")
#Google
plot(Google$N_Pos, Google$Ten_Len, main="Correlation Number of Positions - Tenure: Google",
     xlab="Number of Positions", ylab="Months", col="firebrick1",pch=21)
abline(lm(Google$Ten_Len~Google$N_Pos), col="black")
#Apple
plot(Apple$N_Pos, Apple$Ten_Len, main="Correlation Number of Positions - Tenure: Apple",
     xlab="Number of Positions", ylab="Months", col="darkorchid1",pch=21)
abline(lm(Apple$Ten_Len~Apple$N_Pos), col="black")
#Facebook
plot(Facebook$N_Pos, Facebook$Ten_Len, main="Correlation Number of Positions - Tenure: Facebook",
     xlab="Number of Positions", ylab="Months", col="dodgerblue3",pch=21)
abline(lm(Facebook$Ten_Len~Facebook$N_Pos), col="black")

